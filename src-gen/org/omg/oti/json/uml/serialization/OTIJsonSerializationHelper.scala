/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  Copyright (c) 2015, Airbus Operations S.A.S.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.omg.oti.json.uml.serialization
/**
 * <!-- Start of user code documentation -->
 * <!-- End of user code documentation -->
 */ 

// <!-- Start of user code imports -->
import org.omg.oti._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.json.common._
import org.omg.oti.json.common.OTIPrimitiveTypes.TOOL_SPECIFIC_ID
import org.omg.oti.json.uml._
import org.omg.oti.json.uml.OTIMOFLink._
import org.omg.oti.json.extent._
import org.omg.oti.uml.canonicalXMI.{DocumentOps,DocumentSet}
import org.omg.oti.uml.canonicalXMI.helper.OTIDocumentSetAdapter
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.write.api.{UMLFactory, UMLUpdate}
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.{Boolean, Double, Function1, Int, Option}
import scala.Predef.Integer2int
// <!-- End of user code imports -->

object OTIJsonSerializationHelper {

  // <!-- Start of user code companion -->
  // <!-- End of user code companion -->

}

case class OTIJsonSerializationHelper
[Uml <: UML,
 Uo <: UMLOps[Uml],
 Ch <: OTICharacteristicsProvider[Uml],
 Uf <: UMLFactory[Uml],
 Uu <: UMLUpdate[Uml],
 Do <: DocumentOps[Uml],
 Ds <: DocumentSet[Uml]]
( odsa: OTIDocumentSetAdapter[Uml, Uo, Ch, Uf, Uu, Do, Ds]) {

  // <!-- Start of user code additions -->

  def allExcluded[V <: UMLElement[Uml]]
  (vExcludes: scala.collection.Seq[scala.collection.Iterable[V]])
  : Set[V]
  = ( Set.empty[V] /: vExcludes ) { _ ++ _ }

  def toCompositeLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U, 
   v : V, 
   ctor: (ElementLocation_OTI_ID_OTI_URL, ElementLocation_OTI_ID_OTI_URL) => OTIMOFCompositeLink)
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(v).fold[OTIDocumentExtent](extent) { vd =>
    extent.copy(
      compositeLinkExtent =
        extent.compositeLinkExtent +
          ctor(
            ElementLocation_OTI_ID_OTI_URL(u.toolSpecific_id, ud.info.documentURL),
            ElementLocation_OTI_ID_OTI_URL(v.toolSpecific_id, vd.info.documentURL)))
  }

  def toCompositeLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : scala.collection.Iterable[V],
   ctor: (ElementLocation_OTI_ID_OTI_URL, ElementLocation_OTI_ID_OTI_URL) => OTIMOFCompositeLink,
   vExcludes: scala.collection.Iterable[V]*)
  : OTIDocumentExtent
  = {
    val excluded = allExcluded[V](vExcludes)

    ( extent /: v ) { (ei, uv) =>
      if (excluded.contains(uv))
        ei
      else
        toCompositeLinkExtent(ei, ud, u, uv, ctor)
    }
  }

  // ============

  def toCompositeFirstEndOrderedLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : V,
   vIndex: Int,
   ctor: (ElementLocation_OTI_ID_OTI_URL, Int, ElementLocation_OTI_ID_OTI_URL) => OTIMOFCompositeFirstEndOrderedLink)
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(v).fold[OTIDocumentExtent](extent) { vd =>
    extent.copy(
      compositeFirstEndOrderedLinkExtent =
        extent.compositeFirstEndOrderedLinkExtent +
          ctor(
            ElementLocation_OTI_ID_OTI_URL(u.toolSpecific_id, ud.info.documentURL),
            vIndex,
            ElementLocation_OTI_ID_OTI_URL(v.toolSpecific_id, vd.info.documentURL)))
  }

  def toCompositeFirstEndOrderedLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : Seq[V],
   ctor: (ElementLocation_OTI_ID_OTI_URL, Int, ElementLocation_OTI_ID_OTI_URL) => OTIMOFCompositeFirstEndOrderedLink,
   vExcludes: scala.collection.Iterable[V]*)
  : OTIDocumentExtent
  = {
    val excluded = allExcluded[V](vExcludes)

    ( extent /: v.zipWithIndex ) { case (ei, (uv, uvIndex)) =>
      if (excluded.contains(uv))
        ei
      else
        toCompositeFirstEndOrderedLinkExtent(extent, ud, u, uv, uvIndex, ctor)
    }
  }

  // ============

  def toCompositeSecondEndOrderedLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : V,
   vIndex: Int,
   ctor: (ElementLocation_OTI_ID_OTI_URL, ElementLocation_OTI_ID_OTI_URL, Int) => OTIMOFCompositeSecondEndOrderedLink)
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(v).fold[OTIDocumentExtent](extent) { vd =>
    extent.copy(
      compositeSecondEndOrderedLinkExtent =
        extent.compositeSecondEndOrderedLinkExtent +
          ctor(
            ElementLocation_OTI_ID_OTI_URL(u.toolSpecific_id, ud.info.documentURL),
            ElementLocation_OTI_ID_OTI_URL(v.toolSpecific_id, vd.info.documentURL),
            vIndex))
  }

  def toCompositeSecondEndOrderedLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : scala.collection.Iterable[V],
   ctor: (ElementLocation_OTI_ID_OTI_URL, ElementLocation_OTI_ID_OTI_URL, Int) => OTIMOFCompositeSecondEndOrderedLink,
   vExcludes: scala.collection.Iterable[V]*)
  : OTIDocumentExtent
  = {
    val excluded = allExcluded[V](vExcludes)

    ( extent /: v.zipWithIndex ) { case (ei, (uv, uvIndex)) =>
      if (excluded.contains(uv))
        ei
      else
        toCompositeSecondEndOrderedLinkExtent(extent, ud, u, uv, uvIndex, ctor)
    }
  }

  // ============

  def toReferenceLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : V,
   ctor: (ElementLocation_OTI_ID_OTI_URL, ElementLocation_OTI_ID_OTI_URL) => OTIMOFReferenceLink)
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(v).fold[OTIDocumentExtent](extent) { vd =>
    extent.copy(
      referenceLinkExtent =
        extent.referenceLinkExtent +
          ctor(
            ElementLocation_OTI_ID_OTI_URL(u.toolSpecific_id, ud.info.documentURL),
            ElementLocation_OTI_ID_OTI_URL(v.toolSpecific_id, vd.info.documentURL)))
  }

  def toReferenceLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : scala.collection.Iterable[V],
   ctor: (ElementLocation_OTI_ID_OTI_URL, ElementLocation_OTI_ID_OTI_URL) => OTIMOFReferenceLink,
   vExcludes: scala.collection.Iterable[V]*)
  : OTIDocumentExtent
  = {
    val excluded = allExcluded[V](vExcludes)

    ( extent /: v ) { (ei, uv) =>
      if (excluded.contains(uv))
        ei
      else
        toReferenceLinkExtent(ei, ud, u, uv, ctor)
    }
  }

  // ============

  def toReferenceSecondEndOrderedLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : V,
   vIndex: Int,
   ctor: (ElementLocation_OTI_ID_OTI_URL, ElementLocation_OTI_ID_OTI_URL, Int) => OTIMOFReferenceSecondEndOrderedLink)
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(v).fold[OTIDocumentExtent](extent) { vd =>
    extent.copy(
      referenceSecondEndOrderedLinkExtent =
        extent.referenceSecondEndOrderedLinkExtent +
          ctor(
            ElementLocation_OTI_ID_OTI_URL(u.toolSpecific_id, ud.info.documentURL),
            ElementLocation_OTI_ID_OTI_URL(v.toolSpecific_id, vd.info.documentURL),
            vIndex))
  }

  def toReferenceSecondEndOrderedLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : Seq[V],
   ctor: (ElementLocation_OTI_ID_OTI_URL, ElementLocation_OTI_ID_OTI_URL, Int) => OTIMOFReferenceSecondEndOrderedLink,
   vExcludes: scala.collection.Iterable[V]*)
  : OTIDocumentExtent
  = {
    val excluded = allExcluded[V](vExcludes)

    ( extent /: v.zipWithIndex ) { case (ei, (uv, uvIndex)) =>
      if (excluded.contains(uv))
        ei
      else
        toReferenceSecondEndOrderedLinkExtent(extent, ud, u, uv, uvIndex, ctor)
    }
  }

  // ============

  implicit def optionToOTI[U,V]
  (value: Option[U])
  (implicit u2v: U => V)
  : Option[V]
  = value.map(u2v)

  // <!-- End of user code additions -->

  def addToOTIDocumentExtent
  (extent: OTIDocumentExtent,
   u: UMLElement[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = u match {
    case uu: UMLAcceptCallAction[Uml] => toOTI(extent, uu)
    case uu: UMLActionExecutionSpecification[Uml] => toOTI(extent, uu)
    case uu: UMLActionInputPin[Uml] => toOTI(extent, uu)
    case uu: UMLActivity[Uml] => toOTI(extent, uu)
    case uu: UMLActivityFinalNode[Uml] => toOTI(extent, uu)
    case uu: UMLActivityParameterNode[Uml] => toOTI(extent, uu)
    case uu: UMLActivityPartition[Uml] => toOTI(extent, uu)
    case uu: UMLActor[Uml] => toOTI(extent, uu)
    case uu: UMLAddStructuralFeatureValueAction[Uml] => toOTI(extent, uu)
    case uu: UMLAddVariableValueAction[Uml] => toOTI(extent, uu)
    case uu: UMLAnyReceiveEvent[Uml] => toOTI(extent, uu)
    case uu: UMLAssociationClass[Uml] => toOTI(extent, uu)
    case uu: UMLBehaviorExecutionSpecification[Uml] => toOTI(extent, uu)
    case uu: UMLBroadcastSignalAction[Uml] => toOTI(extent, uu)
    case uu: UMLCallBehaviorAction[Uml] => toOTI(extent, uu)
    case uu: UMLCallEvent[Uml] => toOTI(extent, uu)
    case uu: UMLCallOperationAction[Uml] => toOTI(extent, uu)
    case uu: UMLChangeEvent[Uml] => toOTI(extent, uu)
    case uu: UMLClassifierTemplateParameter[Uml] => toOTI(extent, uu)
    case uu: UMLClause[Uml] => toOTI(extent, uu)
    case uu: UMLClearAssociationAction[Uml] => toOTI(extent, uu)
    case uu: UMLClearStructuralFeatureAction[Uml] => toOTI(extent, uu)
    case uu: UMLClearVariableAction[Uml] => toOTI(extent, uu)
    case uu: UMLCollaboration[Uml] => toOTI(extent, uu)
    case uu: UMLCollaborationUse[Uml] => toOTI(extent, uu)
    case uu: UMLComment[Uml] => toOTI(extent, uu)
    case uu: UMLCommunicationPath[Uml] => toOTI(extent, uu)
    case uu: UMLComponent[Uml] => toOTI(extent, uu)
    case uu: UMLComponentRealization[Uml] => toOTI(extent, uu)
    case uu: UMLConditionalNode[Uml] => toOTI(extent, uu)
    case uu: UMLConnectableElementTemplateParameter[Uml] => toOTI(extent, uu)
    case uu: UMLConnectionPointReference[Uml] => toOTI(extent, uu)
    case uu: UMLConnector[Uml] => toOTI(extent, uu)
    case uu: UMLConnectorEnd[Uml] => toOTI(extent, uu)
    case uu: UMLConsiderIgnoreFragment[Uml] => toOTI(extent, uu)
    case uu: UMLContinuation[Uml] => toOTI(extent, uu)
    case uu: UMLControlFlow[Uml] => toOTI(extent, uu)
    case uu: UMLCreateLinkObjectAction[Uml] => toOTI(extent, uu)
    case uu: UMLCreateObjectAction[Uml] => toOTI(extent, uu)
    case uu: UMLDataStoreNode[Uml] => toOTI(extent, uu)
    case uu: UMLDecisionNode[Uml] => toOTI(extent, uu)
    case uu: UMLDeployment[Uml] => toOTI(extent, uu)
    case uu: UMLDeploymentSpecification[Uml] => toOTI(extent, uu)
    case uu: UMLDestroyLinkAction[Uml] => toOTI(extent, uu)
    case uu: UMLDestroyObjectAction[Uml] => toOTI(extent, uu)
    case uu: UMLDestructionOccurrenceSpecification[Uml] => toOTI(extent, uu)
    case uu: UMLDevice[Uml] => toOTI(extent, uu)
    case uu: UMLDuration[Uml] => toOTI(extent, uu)
    case uu: UMLDurationConstraint[Uml] => toOTI(extent, uu)
    case uu: UMLDurationInterval[Uml] => toOTI(extent, uu)
    case uu: UMLDurationObservation[Uml] => toOTI(extent, uu)
    case uu: UMLElementImport[Uml] => toOTI(extent, uu)
    case uu: UMLElementValue[Uml] => toOTI(extent, uu)
    case uu: UMLEnumeration[Uml] => toOTI(extent, uu)
    case uu: UMLEnumerationLiteral[Uml] => toOTI(extent, uu)
    case uu: UMLExceptionHandler[Uml] => toOTI(extent, uu)
    case uu: UMLExecutionEnvironment[Uml] => toOTI(extent, uu)
    case uu: UMLExecutionOccurrenceSpecification[Uml] => toOTI(extent, uu)
    case uu: UMLExpansionNode[Uml] => toOTI(extent, uu)
    case uu: UMLExpansionRegion[Uml] => toOTI(extent, uu)
    case uu: UMLExtend[Uml] => toOTI(extent, uu)
    case uu: UMLExtension[Uml] => toOTI(extent, uu)
    case uu: UMLExtensionEnd[Uml] => toOTI(extent, uu)
    case uu: UMLExtensionPoint[Uml] => toOTI(extent, uu)
    case uu: UMLFinalState[Uml] => toOTI(extent, uu)
    case uu: UMLFlowFinalNode[Uml] => toOTI(extent, uu)
    case uu: UMLForkNode[Uml] => toOTI(extent, uu)
    case uu: UMLFunctionBehavior[Uml] => toOTI(extent, uu)
    case uu: UMLGate[Uml] => toOTI(extent, uu)
    case uu: UMLGeneralOrdering[Uml] => toOTI(extent, uu)
    case uu: UMLGeneralization[Uml] => toOTI(extent, uu)
    case uu: UMLGeneralizationSet[Uml] => toOTI(extent, uu)
    case uu: UMLImage[Uml] => toOTI(extent, uu)
    case uu: UMLInclude[Uml] => toOTI(extent, uu)
    case uu: UMLInformationFlow[Uml] => toOTI(extent, uu)
    case uu: UMLInformationItem[Uml] => toOTI(extent, uu)
    case uu: UMLInitialNode[Uml] => toOTI(extent, uu)
    case uu: UMLInstanceValue[Uml] => toOTI(extent, uu)
    case uu: UMLInteraction[Uml] => toOTI(extent, uu)
    case uu: UMLInteractionConstraint[Uml] => toOTI(extent, uu)
    case uu: UMLInteractionOperand[Uml] => toOTI(extent, uu)
    case uu: UMLInterface[Uml] => toOTI(extent, uu)
    case uu: UMLInterfaceRealization[Uml] => toOTI(extent, uu)
    case uu: UMLInterruptibleActivityRegion[Uml] => toOTI(extent, uu)
    case uu: UMLJoinNode[Uml] => toOTI(extent, uu)
    case uu: UMLLifeline[Uml] => toOTI(extent, uu)
    case uu: UMLLinkEndCreationData[Uml] => toOTI(extent, uu)
    case uu: UMLLinkEndDestructionData[Uml] => toOTI(extent, uu)
    case uu: UMLLiteralBoolean[Uml] => toOTI(extent, uu)
    case uu: UMLLiteralInteger[Uml] => toOTI(extent, uu)
    case uu: UMLLiteralNull[Uml] => toOTI(extent, uu)
    case uu: UMLLiteralReal[Uml] => toOTI(extent, uu)
    case uu: UMLLiteralString[Uml] => toOTI(extent, uu)
    case uu: UMLLiteralUnlimitedNatural[Uml] => toOTI(extent, uu)
    case uu: UMLLoopNode[Uml] => toOTI(extent, uu)
    case uu: UMLManifestation[Uml] => toOTI(extent, uu)
    case uu: UMLMergeNode[Uml] => toOTI(extent, uu)
    case uu: UMLMessage[Uml] => toOTI(extent, uu)
    case uu: UMLModel[Uml] => toOTI(extent, uu)
    case uu: UMLObjectFlow[Uml] => toOTI(extent, uu)
    case uu: UMLOpaqueAction[Uml] => toOTI(extent, uu)
    case uu: UMLOpaqueExpression[Uml] => toOTI(extent, uu)
    case uu: UMLOperation[Uml] => toOTI(extent, uu)
    case uu: UMLOperationTemplateParameter[Uml] => toOTI(extent, uu)
    case uu: UMLOutputPin[Uml] => toOTI(extent, uu)
    case uu: UMLPackageImport[Uml] => toOTI(extent, uu)
    case uu: UMLPackageMerge[Uml] => toOTI(extent, uu)
    case uu: UMLParameter[Uml] => toOTI(extent, uu)
    case uu: UMLParameterSet[Uml] => toOTI(extent, uu)
    case uu: UMLPartDecomposition[Uml] => toOTI(extent, uu)
    case uu: UMLPort[Uml] => toOTI(extent, uu)
    case uu: UMLPrimitiveType[Uml] => toOTI(extent, uu)
    case uu: UMLProfile[Uml] => toOTI(extent, uu)
    case uu: UMLProfileApplication[Uml] => toOTI(extent, uu)
    case uu: UMLProtocolConformance[Uml] => toOTI(extent, uu)
    case uu: UMLProtocolStateMachine[Uml] => toOTI(extent, uu)
    case uu: UMLProtocolTransition[Uml] => toOTI(extent, uu)
    case uu: UMLPseudostate[Uml] => toOTI(extent, uu)
    case uu: UMLQualifierValue[Uml] => toOTI(extent, uu)
    case uu: UMLRaiseExceptionAction[Uml] => toOTI(extent, uu)
    case uu: UMLReadExtentAction[Uml] => toOTI(extent, uu)
    case uu: UMLReadIsClassifiedObjectAction[Uml] => toOTI(extent, uu)
    case uu: UMLReadLinkAction[Uml] => toOTI(extent, uu)
    case uu: UMLReadLinkObjectEndAction[Uml] => toOTI(extent, uu)
    case uu: UMLReadLinkObjectEndQualifierAction[Uml] => toOTI(extent, uu)
    case uu: UMLReadSelfAction[Uml] => toOTI(extent, uu)
    case uu: UMLReadStructuralFeatureAction[Uml] => toOTI(extent, uu)
    case uu: UMLReadVariableAction[Uml] => toOTI(extent, uu)
    case uu: UMLReception[Uml] => toOTI(extent, uu)
    case uu: UMLReclassifyObjectAction[Uml] => toOTI(extent, uu)
    case uu: UMLRedefinableTemplateSignature[Uml] => toOTI(extent, uu)
    case uu: UMLReduceAction[Uml] => toOTI(extent, uu)
    case uu: UMLRegion[Uml] => toOTI(extent, uu)
    case uu: UMLRemoveStructuralFeatureValueAction[Uml] => toOTI(extent, uu)
    case uu: UMLRemoveVariableValueAction[Uml] => toOTI(extent, uu)
    case uu: UMLReplyAction[Uml] => toOTI(extent, uu)
    case uu: UMLSendObjectAction[Uml] => toOTI(extent, uu)
    case uu: UMLSendSignalAction[Uml] => toOTI(extent, uu)
    case uu: UMLSequenceNode[Uml] => toOTI(extent, uu)
    case uu: UMLSignal[Uml] => toOTI(extent, uu)
    case uu: UMLSignalEvent[Uml] => toOTI(extent, uu)
    case uu: UMLSlot[Uml] => toOTI(extent, uu)
    case uu: UMLStartClassifierBehaviorAction[Uml] => toOTI(extent, uu)
    case uu: UMLStartObjectBehaviorAction[Uml] => toOTI(extent, uu)
    case uu: UMLStateInvariant[Uml] => toOTI(extent, uu)
    case uu: UMLStereotype[Uml] => toOTI(extent, uu)
    case uu: UMLStringExpression[Uml] => toOTI(extent, uu)
    case uu: UMLSubstitution[Uml] => toOTI(extent, uu)
    case uu: UMLTemplateBinding[Uml] => toOTI(extent, uu)
    case uu: UMLTemplateParameterSubstitution[Uml] => toOTI(extent, uu)
    case uu: UMLTestIdentityAction[Uml] => toOTI(extent, uu)
    case uu: UMLTimeConstraint[Uml] => toOTI(extent, uu)
    case uu: UMLTimeEvent[Uml] => toOTI(extent, uu)
    case uu: UMLTimeExpression[Uml] => toOTI(extent, uu)
    case uu: UMLTimeInterval[Uml] => toOTI(extent, uu)
    case uu: UMLTimeObservation[Uml] => toOTI(extent, uu)
    case uu: UMLTrigger[Uml] => toOTI(extent, uu)
    case uu: UMLUnmarshallAction[Uml] => toOTI(extent, uu)
    case uu: UMLUsage[Uml] => toOTI(extent, uu)
    case uu: UMLUseCase[Uml] => toOTI(extent, uu)
    case uu: UMLValuePin[Uml] => toOTI(extent, uu)
    case uu: UMLValueSpecificationAction[Uml] => toOTI(extent, uu)
    case uu: UMLVariable[Uml] => toOTI(extent, uu)
    case uu: UMLAcceptEventAction[Uml] => toOTI(extent, uu)
    case uu: UMLArtifact[Uml] => toOTI(extent, uu)
    case uu: UMLAssociation[Uml] => toOTI(extent, uu)
    case uu: UMLCentralBufferNode[Uml] => toOTI(extent, uu)
    case uu: UMLCombinedFragment[Uml] => toOTI(extent, uu)
    case uu: UMLCreateLinkAction[Uml] => toOTI(extent, uu)
    case uu: UMLDataType[Uml] => toOTI(extent, uu)
    case uu: UMLExpression[Uml] => toOTI(extent, uu)
    case uu: UMLInputPin[Uml] => toOTI(extent, uu)
    case uu: UMLInstanceSpecification[Uml] => toOTI(extent, uu)
    case uu: UMLInteractionUse[Uml] => toOTI(extent, uu)
    case uu: UMLInterval[Uml] => toOTI(extent, uu)
    case uu: UMLIntervalConstraint[Uml] => toOTI(extent, uu)
    case uu: UMLLinkEndData[Uml] => toOTI(extent, uu)
    case uu: UMLMessageOccurrenceSpecification[Uml] => toOTI(extent, uu)
    case uu: UMLNode[Uml] => toOTI(extent, uu)
    case uu: UMLOpaqueBehavior[Uml] => toOTI(extent, uu)
    case uu: UMLPackage[Uml] => toOTI(extent, uu)
    case uu: UMLProperty[Uml] => toOTI(extent, uu)
    case uu: UMLRealization[Uml] => toOTI(extent, uu)
    case uu: UMLState[Uml] => toOTI(extent, uu)
    case uu: UMLStateMachine[Uml] => toOTI(extent, uu)
    case uu: UMLStructuredActivityNode[Uml] => toOTI(extent, uu)
    case uu: UMLTemplateParameter[Uml] => toOTI(extent, uu)
    case uu: UMLTemplateSignature[Uml] => toOTI(extent, uu)
    case uu: UMLTransition[Uml] => toOTI(extent, uu)
    case uu: UMLAbstraction[Uml] => toOTI(extent, uu)
    case uu: UMLClass[Uml] => toOTI(extent, uu)
    case uu: UMLConstraint[Uml] => toOTI(extent, uu)
    case uu: UMLOccurrenceSpecification[Uml] => toOTI(extent, uu)
    case uu: UMLDependency[Uml] => toOTI(extent, uu)
    case _ => extent
  }

  implicit def toOTI
  (value: uml.read.api.UMLAggregationKind.UMLAggregationKind)
  : json.uml.enums.UMLAggregationKind
  = value match {
    case uml.read.api.UMLAggregationKind.composite =>
      json.uml.enums.UMLAggregationKind.composite

    case uml.read.api.UMLAggregationKind.none =>
      json.uml.enums.UMLAggregationKind.none

    case uml.read.api.UMLAggregationKind.shared =>
      json.uml.enums.UMLAggregationKind.shared
  }

  implicit def toOTI
  (value: uml.read.api.UMLCallConcurrencyKind.UMLCallConcurrencyKind)
  : json.uml.enums.UMLCallConcurrencyKind
  = value match {
    case uml.read.api.UMLCallConcurrencyKind.concurrent =>
      json.uml.enums.UMLCallConcurrencyKind.concurrent

    case uml.read.api.UMLCallConcurrencyKind.guarded =>
      json.uml.enums.UMLCallConcurrencyKind.guarded

    case uml.read.api.UMLCallConcurrencyKind.sequential =>
      json.uml.enums.UMLCallConcurrencyKind.sequential
  }

  implicit def toOTI
  (value: uml.read.api.UMLConnectorKind.UMLConnectorKind)
  : json.uml.enums.UMLConnectorKind
  = value match {
    case uml.read.api.UMLConnectorKind.assembly =>
      json.uml.enums.UMLConnectorKind.assembly

    case uml.read.api.UMLConnectorKind.delegation =>
      json.uml.enums.UMLConnectorKind.delegation
  }

  implicit def toOTI
  (value: uml.read.api.UMLExpansionKind.UMLExpansionKind)
  : json.uml.enums.UMLExpansionKind
  = value match {
    case uml.read.api.UMLExpansionKind.iterative =>
      json.uml.enums.UMLExpansionKind.iterative

    case uml.read.api.UMLExpansionKind.parallel =>
      json.uml.enums.UMLExpansionKind.parallel

    case uml.read.api.UMLExpansionKind.stream =>
      json.uml.enums.UMLExpansionKind.stream
  }

  implicit def toOTI
  (value: uml.read.api.UMLInteractionOperatorKind.UMLInteractionOperatorKind)
  : json.uml.enums.UMLInteractionOperatorKind
  = value match {
    case uml.read.api.UMLInteractionOperatorKind.alt =>
      json.uml.enums.UMLInteractionOperatorKind.alt

    case uml.read.api.UMLInteractionOperatorKind.assert =>
      json.uml.enums.UMLInteractionOperatorKind.assert

    case uml.read.api.UMLInteractionOperatorKind.break =>
      json.uml.enums.UMLInteractionOperatorKind.break

    case uml.read.api.UMLInteractionOperatorKind.consider =>
      json.uml.enums.UMLInteractionOperatorKind.consider

    case uml.read.api.UMLInteractionOperatorKind.critical =>
      json.uml.enums.UMLInteractionOperatorKind.critical

    case uml.read.api.UMLInteractionOperatorKind.ignore =>
      json.uml.enums.UMLInteractionOperatorKind.ignore

    case uml.read.api.UMLInteractionOperatorKind.loop =>
      json.uml.enums.UMLInteractionOperatorKind.loop

    case uml.read.api.UMLInteractionOperatorKind.neg =>
      json.uml.enums.UMLInteractionOperatorKind.neg

    case uml.read.api.UMLInteractionOperatorKind.opt =>
      json.uml.enums.UMLInteractionOperatorKind.opt

    case uml.read.api.UMLInteractionOperatorKind.par =>
      json.uml.enums.UMLInteractionOperatorKind.par

    case uml.read.api.UMLInteractionOperatorKind.seq =>
      json.uml.enums.UMLInteractionOperatorKind.seq

    case uml.read.api.UMLInteractionOperatorKind.strict =>
      json.uml.enums.UMLInteractionOperatorKind.strict
  }

  implicit def toOTI
  (value: uml.read.api.UMLMessageKind.UMLMessageKind)
  : json.uml.enums.UMLMessageKind
  = value match {
    case uml.read.api.UMLMessageKind.complete =>
      json.uml.enums.UMLMessageKind.complete

    case uml.read.api.UMLMessageKind.found =>
      json.uml.enums.UMLMessageKind.found

    case uml.read.api.UMLMessageKind.lost =>
      json.uml.enums.UMLMessageKind.lost

    case uml.read.api.UMLMessageKind.unknown =>
      json.uml.enums.UMLMessageKind.unknown
  }

  implicit def toOTI
  (value: uml.read.api.UMLMessageSort.UMLMessageSort)
  : json.uml.enums.UMLMessageSort
  = value match {
    case uml.read.api.UMLMessageSort.asynchCall =>
      json.uml.enums.UMLMessageSort.asynchCall

    case uml.read.api.UMLMessageSort.asynchSignal =>
      json.uml.enums.UMLMessageSort.asynchSignal

    case uml.read.api.UMLMessageSort.createMessage =>
      json.uml.enums.UMLMessageSort.createMessage

    case uml.read.api.UMLMessageSort.deleteMessage =>
      json.uml.enums.UMLMessageSort.deleteMessage

    case uml.read.api.UMLMessageSort.reply =>
      json.uml.enums.UMLMessageSort.reply

    case uml.read.api.UMLMessageSort.synchCall =>
      json.uml.enums.UMLMessageSort.synchCall
  }

  implicit def toOTI
  (value: uml.read.api.UMLObjectNodeOrderingKind.UMLObjectNodeOrderingKind)
  : json.uml.enums.UMLObjectNodeOrderingKind
  = value match {
    case uml.read.api.UMLObjectNodeOrderingKind.FIFO =>
      json.uml.enums.UMLObjectNodeOrderingKind.FIFO

    case uml.read.api.UMLObjectNodeOrderingKind.LIFO =>
      json.uml.enums.UMLObjectNodeOrderingKind.LIFO

    case uml.read.api.UMLObjectNodeOrderingKind.ordered =>
      json.uml.enums.UMLObjectNodeOrderingKind.ordered

    case uml.read.api.UMLObjectNodeOrderingKind.unordered =>
      json.uml.enums.UMLObjectNodeOrderingKind.unordered
  }

  implicit def toOTI
  (value: uml.read.api.UMLParameterDirectionKind.UMLParameterDirectionKind)
  : json.uml.enums.UMLParameterDirectionKind
  = value match {
    case uml.read.api.UMLParameterDirectionKind._return =>
      json.uml.enums.UMLParameterDirectionKind._return

    case uml.read.api.UMLParameterDirectionKind.in =>
      json.uml.enums.UMLParameterDirectionKind.in

    case uml.read.api.UMLParameterDirectionKind.inout =>
      json.uml.enums.UMLParameterDirectionKind.inout

    case uml.read.api.UMLParameterDirectionKind.out =>
      json.uml.enums.UMLParameterDirectionKind.out
  }

  implicit def toOTI
  (value: uml.read.api.UMLParameterEffectKind.UMLParameterEffectKind)
  : json.uml.enums.UMLParameterEffectKind
  = value match {
    case uml.read.api.UMLParameterEffectKind.create =>
      json.uml.enums.UMLParameterEffectKind.create

    case uml.read.api.UMLParameterEffectKind.delete =>
      json.uml.enums.UMLParameterEffectKind.delete

    case uml.read.api.UMLParameterEffectKind.read =>
      json.uml.enums.UMLParameterEffectKind.read

    case uml.read.api.UMLParameterEffectKind.update =>
      json.uml.enums.UMLParameterEffectKind.update
  }

  implicit def toOTI
  (value: uml.read.api.UMLPseudostateKind.UMLPseudostateKind)
  : json.uml.enums.UMLPseudostateKind
  = value match {
    case uml.read.api.UMLPseudostateKind.choice =>
      json.uml.enums.UMLPseudostateKind.choice

    case uml.read.api.UMLPseudostateKind.deepHistory =>
      json.uml.enums.UMLPseudostateKind.deepHistory

    case uml.read.api.UMLPseudostateKind.entryPoint =>
      json.uml.enums.UMLPseudostateKind.entryPoint

    case uml.read.api.UMLPseudostateKind.exitPoint =>
      json.uml.enums.UMLPseudostateKind.exitPoint

    case uml.read.api.UMLPseudostateKind.fork =>
      json.uml.enums.UMLPseudostateKind.fork

    case uml.read.api.UMLPseudostateKind.initial =>
      json.uml.enums.UMLPseudostateKind.initial

    case uml.read.api.UMLPseudostateKind.join =>
      json.uml.enums.UMLPseudostateKind.join

    case uml.read.api.UMLPseudostateKind.junction =>
      json.uml.enums.UMLPseudostateKind.junction

    case uml.read.api.UMLPseudostateKind.shallowHistory =>
      json.uml.enums.UMLPseudostateKind.shallowHistory

    case uml.read.api.UMLPseudostateKind.terminate =>
      json.uml.enums.UMLPseudostateKind.terminate
  }

  implicit def toOTI
  (value: uml.read.api.UMLTransitionKind.UMLTransitionKind)
  : json.uml.enums.UMLTransitionKind
  = value match {
    case uml.read.api.UMLTransitionKind.external =>
      json.uml.enums.UMLTransitionKind.external

    case uml.read.api.UMLTransitionKind.internal =>
      json.uml.enums.UMLTransitionKind.internal

    case uml.read.api.UMLTransitionKind.local =>
      json.uml.enums.UMLTransitionKind.local
  }

  implicit def toOTI
  (value: uml.read.api.UMLVisibilityKind.UMLVisibilityKind)
  : json.uml.enums.UMLVisibilityKind
  = value match {
    case uml.read.api.UMLVisibilityKind._package =>
      json.uml.enums.UMLVisibilityKind._package

    case uml.read.api.UMLVisibilityKind._private =>
      json.uml.enums.UMLVisibilityKind._private

    case uml.read.api.UMLVisibilityKind._protected =>
      json.uml.enums.UMLVisibilityKind._protected

    case uml.read.api.UMLVisibilityKind.public =>
      json.uml.enums.UMLVisibilityKind.public
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLAbstraction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLAbstraction(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLAcceptCallAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLAcceptCallAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isUnmarshall = u.isUnmarshall,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.result, OTIUMLA_result_acceptEventAction)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.returnInformation, OTIUMLA_returnInformation_acceptCallAction)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.trigger, OTIUMLA_trigger_acceptEventAction)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLAcceptEventAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLAcceptEventAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isUnmarshall = u.isUnmarshall,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.result, OTIUMLA_result_acceptEventAction)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.trigger, OTIUMLA_trigger_acceptEventAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLActionExecutionSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLActionExecutionSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.covered, OTIUMLA_covered_coveredBy)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLActionInputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLActionInputPin(
            toolSpecific_id = u.toolSpecific_id,
            isControl = u.isControl,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            isOrdered = u.isOrdered,
            isUnique = u.isUnique,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.fromAction, OTIUMLA_fromAction_actionInputPin)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.inState, OTIUMLA_inState_objectNode)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLActivity[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLActivity(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            isReadOnly = u.isReadOnly,
            isReentrant = u.isReentrant,
            isSingleExecution = u.isSingleExecution,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.edge, OTIUMLA_edge_activity)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.group, OTIUMLA_group_inActivity, u.structuredNode)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e8 = 
      toCompositeSecondEndOrderedLinkExtent(e7, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.node, OTIUMLA_node_activity, u.structuredNode)
    val e10 = 
      toCompositeFirstEndOrderedLinkExtent(e9, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e14 = 
      toCompositeSecondEndOrderedLinkExtent(e13, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e15 = 
      toCompositeSecondEndOrderedLinkExtent(e14, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.ownedRule, OTIUMLA_ownedRule_context, u.postcondition, u.precondition)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.postcondition, OTIUMLA_postcondition_behavior)
    val e23 = 
      toCompositeLinkExtent(e22, ud, u, u.precondition, OTIUMLA_precondition_behavior)
    val e24 = 
      toCompositeLinkExtent(e23, ud, u, u.structuredNode, OTIUMLA_structuredNode_activity)
    val e25 = 
      toCompositeLinkExtent(e24, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e26 = 
      toCompositeLinkExtent(e25, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e27 = 
      toCompositeLinkExtent(e26, ud, u, u.variable, OTIUMLA_variable_activityScope)
    val e28 =
      toReferenceLinkExtent(e27, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e29 =
      toReferenceLinkExtent(e28, ud, u, u.partition, OTIUMLA_partition_activity)
    val e30 =
      toReferenceLinkExtent(e29, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e31 =
      toReferenceLinkExtent(e30, ud, u, u.redefinedBehavior, OTIUMLA_redefinedBehavior_behavior)
    val e32 =
      toReferenceLinkExtent(e31, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier, u.redefinedBehavior)
    val e33 =
      toReferenceLinkExtent(e32, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e34 =
      toReferenceLinkExtent(e33, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e34  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLActivityFinalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLActivityFinalNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLActivityParameterNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLActivityParameterNode(
            toolSpecific_id = u.toolSpecific_id,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.inState, OTIUMLA_inState_objectNode)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLActivityPartition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLActivityPartition(
            toolSpecific_id = u.toolSpecific_id,
            isDimension = u.isDimension,
            isExternal = u.isExternal,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.subpartition, OTIUMLA_subpartition_superPartition)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.edge, OTIUMLA_edge_inPartition)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.node, OTIUMLA_inPartition_node)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLActor[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLActor(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e18  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLAddStructuralFeatureValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLAddStructuralFeatureValueAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isReplaceAll = u.isReplaceAll,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.insertAt, OTIUMLA_insertAt_addStructuralFeatureValueAction)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u._object, OTIUMLA_object_structuralFeatureAction)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.result, OTIUMLA_result_writeStructuralFeatureAction)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.value, OTIUMLA_value_writeStructuralFeatureAction)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e12 =
      toReferenceLinkExtent(e11, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e12  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLAddVariableValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLAddVariableValueAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isReplaceAll = u.isReplaceAll,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.insertAt, OTIUMLA_insertAt_addVariableValueAction)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.value, OTIUMLA_value_writeVariableAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLAnyReceiveEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLAnyReceiveEvent(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLArtifact[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLArtifact(
            toolSpecific_id = u.toolSpecific_id,
            fileName = u.fileName,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.manifestation, OTIUMLA_manifestation_artifact)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nestedArtifact, OTIUMLA_nestedArtifact_artifact)
    val e7 = 
      toCompositeSecondEndOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_artifact)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e9 = 
      toCompositeSecondEndOrderedLinkExtent(e8, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_artifact)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e19  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLAssociation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLAssociation(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isDerived = u.isDerived,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeFirstEndOrderedLinkExtent(e5, ud, u, u.ownedEnd, OTIUMLA_ownedEnd_owningAssociation)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e13 =
      toReferenceFirstEndOrderedLinkExtent(e12, ud, u, u.memberEnd, OTIUMLA_memberEnd_association, u.navigableOwnedEnd)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.navigableOwnedEnd, OTIUMLA_navigableOwnedEnd_association)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e18  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLAssociationClass[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLAssociationClass(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isDerived = u.isDerived,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e7 = 
      toCompositeFirstEndOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e11 = 
      toCompositeFirstEndOrderedLinkExtent(e10, ud, u, u.ownedEnd, OTIUMLA_ownedEnd_owningAssociation)
    val e12 = 
      toCompositeSecondEndOrderedLinkExtent(e11, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e21 =
      toReferenceFirstEndOrderedLinkExtent(e20, ud, u, u.memberEnd, OTIUMLA_memberEnd_association, u.navigableOwnedEnd)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.navigableOwnedEnd, OTIUMLA_navigableOwnedEnd_association)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e26 =
      toReferenceLinkExtent(e25, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e26  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLBehaviorExecutionSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLBehaviorExecutionSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.covered, OTIUMLA_covered_coveredBy)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLBroadcastSignalAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLBroadcastSignalAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeSecondEndOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_invocationAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e9  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLCallBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLCallBehaviorAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isSynchronous = u.isSynchronous,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeSecondEndOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_invocationAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeSecondEndOrderedLinkExtent(e6, ud, u, u.result, OTIUMLA_result_callAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLCallEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLCallEvent(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLCallOperationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLCallOperationAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isSynchronous = u.isSynchronous,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeSecondEndOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_invocationAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeSecondEndOrderedLinkExtent(e6, ud, u, u.result, OTIUMLA_result_callAction)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.target, OTIUMLA_target_callOperationAction)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLCentralBufferNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLCentralBufferNode(
            toolSpecific_id = u.toolSpecific_id,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.inState, OTIUMLA_inState_objectNode)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLChangeEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLChangeEvent(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.changeExpression, OTIUMLA_changeExpression_changeEvent)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLClass[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLClass(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e7 = 
      toCompositeFirstEndOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e11 = 
      toCompositeSecondEndOrderedLinkExtent(e10, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e23  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLClassifierTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLClassifierTemplateParameter(
            toolSpecific_id = u.toolSpecific_id,
            allowSubstitutable = u.allowSubstitutable))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.constrainingClassifier, OTIUMLA_constrainingClassifier_classifierTemplateParameter)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLClause[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLClause(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 =
      toReferenceLinkExtent(e1, ud, u, u.body, OTIUMLA_body_clause)
    val e3 =
      toReferenceSecondEndOrderedLinkExtent(e2, ud, u, u.bodyOutput, OTIUMLA_bodyOutput_clause)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.decider, OTIUMLA_decider_clause)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.predecessorClause, OTIUMLA_predecessorClause_successorClause)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.test, OTIUMLA_test_clause)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLClearAssociationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLClearAssociationAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_clearAssociationAction)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.association, OTIUMLA_association_clearAssociationAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLClearStructuralFeatureAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLClearStructuralFeatureAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_structuralFeatureAction)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_clearStructuralFeatureAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLClearVariableAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLClearVariableAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e8  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLCollaboration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLCollaboration(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_structuredClassifier)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.collaborationRole, OTIUMLA_collaborationRole_collaboration)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e21  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLCollaborationUse[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLCollaborationUse(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.roleBinding, OTIUMLA_roleBinding_collaborationUse)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLCombinedFragment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLCombinedFragment(
            toolSpecific_id = u.toolSpecific_id,
            interactionOperator = u.interactionOperator,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.cfragmentGate, OTIUMLA_cfragmentGate_combinedFragment)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e4 = 
      toCompositeSecondEndOrderedLinkExtent(e3, ud, u, u.operand, OTIUMLA_operand_combinedFragment)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.covered, OTIUMLA_covered_coveredBy)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLComment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLComment(
            toolSpecific_id = u.toolSpecific_id,
            body = u.body))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 =
      toReferenceLinkExtent(e1, ud, u, u.annotatedElement, OTIUMLA_annotatedElement_comment)
    val result = e2  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLCommunicationPath[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLCommunicationPath(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isDerived = u.isDerived,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeFirstEndOrderedLinkExtent(e5, ud, u, u.ownedEnd, OTIUMLA_ownedEnd_owningAssociation)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e13 =
      toReferenceFirstEndOrderedLinkExtent(e12, ud, u, u.memberEnd, OTIUMLA_memberEnd_association, u.navigableOwnedEnd)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.navigableOwnedEnd, OTIUMLA_navigableOwnedEnd_association)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e18  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLComponent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLComponent(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isIndirectlyInstantiated = u.isIndirectlyInstantiated,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e7 = 
      toCompositeFirstEndOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e11 = 
      toCompositeSecondEndOrderedLinkExtent(e10, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.packagedElement, OTIUMLA_packagedElement_component)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.realization, OTIUMLA_realization_abstraction_component)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e25  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLComponentRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLComponentRealization(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client, u.realizingClassifier)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.realizingClassifier, OTIUMLA_realizingClassifier_componentRealization)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLConditionalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLConditionalNode(
            toolSpecific_id = u.toolSpecific_id,
            isAssured = u.isAssured,
            isDeterminate = u.isDeterminate,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            mustIsolate = u.mustIsolate,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.clause, OTIUMLA_clause_conditionalNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.edge, OTIUMLA_edge_inStructuredNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.node, OTIUMLA_node_inStructuredNode)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e12 = 
      toCompositeSecondEndOrderedLinkExtent(e11, ud, u, u.result, OTIUMLA_result_conditionalNode)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.variable, OTIUMLA_variable_scope)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e17  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLConnectableElementTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLConnectableElementTemplateParameter(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLConnectionPointReference[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLConnectionPointReference(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.entry, OTIUMLA_entry_connectionPointReference)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.exit, OTIUMLA_exit_connectionPointReference)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLConnector[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLConnector(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isStatic = u.isStatic,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeSecondEndOrderedLinkExtent(e0, ud, u, u.end, OTIUMLA_end_connector)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.contract, OTIUMLA_contract_connector)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedConnector, OTIUMLA_redefinedConnector_connector)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLConnectorEnd[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLConnectorEnd(
            toolSpecific_id = u.toolSpecific_id,
            isOrdered = u.isOrdered,
            isUnique = u.isUnique))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLConsiderIgnoreFragment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLConsiderIgnoreFragment(
            toolSpecific_id = u.toolSpecific_id,
            interactionOperator = u.interactionOperator,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.cfragmentGate, OTIUMLA_cfragmentGate_combinedFragment)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e4 = 
      toCompositeSecondEndOrderedLinkExtent(e3, ud, u, u.operand, OTIUMLA_operand_combinedFragment)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.covered, OTIUMLA_covered_coveredBy)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.message, OTIUMLA_message_considerIgnoreFragment)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLConstraint(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.specification, OTIUMLA_specification_owningConstraint)
    val e4 =
      toReferenceSecondEndOrderedLinkExtent(e3, ud, u, u.constrainedElement, OTIUMLA_constrainedElement_constraint)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLContinuation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLContinuation(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            setting = u.setting,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.covered, OTIUMLA_covered_coveredBy)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLControlFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLControlFlow(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.guard, OTIUMLA_guard_activityEdge)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.weight, OTIUMLA_weight_activityEdge)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedEdge, OTIUMLA_redefinedEdge_activityEdge)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLCreateLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLCreateLinkAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.endData, OTIUMLA_endData_createLinkAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.inputValue, OTIUMLA_inputValue_linkAction)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLCreateLinkObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLCreateLinkObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.endData, OTIUMLA_endData_createLinkAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.inputValue, OTIUMLA_inputValue_linkAction)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.result, OTIUMLA_result_createLinkObjectAction)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLCreateObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLCreateObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.result, OTIUMLA_result_createObjectAction)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e9  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDataStoreNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDataStoreNode(
            toolSpecific_id = u.toolSpecific_id,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.inState, OTIUMLA_inState_objectNode)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDataType[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDataType(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeFirstEndOrderedLinkExtent(e4, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_datatype)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeFirstEndOrderedLinkExtent(e6, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_datatype)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e17  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDecisionNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDecisionNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.decisionInputFlow, OTIUMLA_decisionInputFlow_decisionNode)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDependency[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDependency(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.client, OTIUMLA_clientDependency_client)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDeployment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDeployment(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.configuration, OTIUMLA_configuration_deployment)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.deployedArtifact, OTIUMLA_deployedArtifact_deploymentForArtifact)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency, u.deployedArtifact)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDeploymentSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDeploymentSpecification(
            toolSpecific_id = u.toolSpecific_id,
            deploymentLocation = u.deploymentLocation,
            executionLocation = u.executionLocation,
            fileName = u.fileName,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.manifestation, OTIUMLA_manifestation_artifact)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nestedArtifact, OTIUMLA_nestedArtifact_artifact)
    val e7 = 
      toCompositeSecondEndOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_artifact)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e9 = 
      toCompositeSecondEndOrderedLinkExtent(e8, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_artifact)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e19  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDestroyLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDestroyLinkAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.endData, OTIUMLA_endData_destroyLinkAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.inputValue, OTIUMLA_inputValue_linkAction)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDestroyObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDestroyObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isDestroyLinks = u.isDestroyLinks,
            isDestroyOwnedObjects = u.isDestroyOwnedObjects,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.target, OTIUMLA_target_destroyObjectAction)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e9  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDestructionOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDestructionOccurrenceSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.message, OTIUMLA_message_messageEnd)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.toAfter, OTIUMLA_before_toAfter)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.toBefore, OTIUMLA_toBefore_after)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDevice[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDevice(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e7 = 
      toCompositeSecondEndOrderedLinkExtent(e6, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nestedNode, OTIUMLA_nestedNode_node)
    val e9 = 
      toCompositeFirstEndOrderedLinkExtent(e8, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e13 = 
      toCompositeSecondEndOrderedLinkExtent(e12, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e25  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDuration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDuration(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.expr, OTIUMLA_expr_duration)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.observation, OTIUMLA_observation_duration)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDurationConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDurationConstraint(
            toolSpecific_id = u.toolSpecific_id,
            firstEvent = u.firstEvent,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.specification, OTIUMLA_specification_durationConstraint)
    val e4 =
      toReferenceSecondEndOrderedLinkExtent(e3, ud, u, u.constrainedElement, OTIUMLA_constrainedElement_constraint)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDurationInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDurationInterval(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLDurationObservation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLDurationObservation(
            toolSpecific_id = u.toolSpecific_id,
            firstEvent = u.firstEvent,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceSecondEndOrderedLinkExtent(e2, ud, u, u.event, OTIUMLA_event_durationObservation)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLElementImport[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLElementImport(
            toolSpecific_id = u.toolSpecific_id,
            alias = u.alias,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e1  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLElementValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLElementValue(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLEnumeration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLEnumeration(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeFirstEndOrderedLinkExtent(e4, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_datatype)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeFirstEndOrderedLinkExtent(e6, ud, u, u.ownedLiteral, OTIUMLA_ownedLiteral_enumeration)
    val e8 = 
      toCompositeFirstEndOrderedLinkExtent(e7, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_datatype)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e18  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLEnumerationLiteral[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLEnumerationLiteral(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.deployment, OTIUMLA_deployment_location)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.slot, OTIUMLA_slot_owningInstance)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.specification, OTIUMLA_specification_owningInstanceSpec)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLExceptionHandler[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLExceptionHandler(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 =
      toReferenceLinkExtent(e1, ud, u, u.exceptionType, OTIUMLA_exceptionType_exceptionHandler)
    val result = e2  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLExecutionEnvironment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLExecutionEnvironment(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e7 = 
      toCompositeSecondEndOrderedLinkExtent(e6, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nestedNode, OTIUMLA_nestedNode_node)
    val e9 = 
      toCompositeFirstEndOrderedLinkExtent(e8, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e13 = 
      toCompositeSecondEndOrderedLinkExtent(e12, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e25  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLExecutionOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLExecutionOccurrenceSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.execution, OTIUMLA_execution_executionOccurrenceSpecification)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.toAfter, OTIUMLA_before_toAfter)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.toBefore, OTIUMLA_toBefore_after)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLExpansionNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLExpansionNode(
            toolSpecific_id = u.toolSpecific_id,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.inState, OTIUMLA_inState_objectNode)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLExpansionRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLExpansionRegion(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            mode = u.mode,
            mustIsolate = u.mustIsolate,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.edge, OTIUMLA_edge_inStructuredNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.node, OTIUMLA_node_inStructuredNode)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.structuredNodeOutput, OTIUMLA_structuredNodeOutput_structuredActivityNode)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.variable, OTIUMLA_variable_scope)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.inputElement, OTIUMLA_inputElement_regionAsInput)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.outputElement, OTIUMLA_outputElement_regionAsOutput)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e18  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLExpression(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            symbol = u.symbol,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeSecondEndOrderedLinkExtent(e1, ud, u, u.operand, OTIUMLA_operand_expression)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLExtend[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLExtend(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.condition, OTIUMLA_condition_extend)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceSecondEndOrderedLinkExtent(e3, ud, u, u.extensionLocation, OTIUMLA_extensionLocation_extension)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLExtension[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLExtension(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isDerived = u.isDerived,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedEnd, OTIUMLA_ownedEnd_extension)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e13 =
      toReferenceFirstEndOrderedLinkExtent(e12, ud, u, u.memberEnd, OTIUMLA_memberEnd_association, u.navigableOwnedEnd)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.navigableOwnedEnd, OTIUMLA_navigableOwnedEnd_association)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e18  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLExtensionEnd[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLExtensionEnd(
            toolSpecific_id = u.toolSpecific_id,
            aggregation = u.aggregation,
            isDerived = u.isDerived,
            isDerivedUnion = u.isDerivedUnion,
            isID = u.isID,
            isLeaf = u.isLeaf,
            isOrdered = u.isOrdered,
            isReadOnly = u.isReadOnly,
            isStatic = u.isStatic,
            isUnique = u.isUnique,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.defaultValue, OTIUMLA_defaultValue_owningProperty)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.qualifier, OTIUMLA_qualifier_associationEnd)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.redefinedProperty, OTIUMLA_redefinedProperty_property)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.subsettedProperty, OTIUMLA_subsettedProperty_property)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLExtensionPoint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLExtensionPoint(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e2  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLFinalState[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLFinalState(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.connection, OTIUMLA_connection_state)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.connectionPoint, OTIUMLA_connectionPoint_state)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.deferrableTrigger, OTIUMLA_deferrableTrigger_state)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.doActivity, OTIUMLA_doActivity_state)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.entry, OTIUMLA_entry_state)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.exit, OTIUMLA_exit_state)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context, u.stateInvariant)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.region, OTIUMLA_region_state)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.stateInvariant, OTIUMLA_stateInvariant_owningState)
    val result = e13  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLFlowFinalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLFlowFinalNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLForkNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLForkNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLFunctionBehavior[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLFunctionBehavior(
            toolSpecific_id = u.toolSpecific_id,
            body = u.body,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            isReentrant = u.isReentrant,
            language = u.language,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e7 = 
      toCompositeFirstEndOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e11 = 
      toCompositeSecondEndOrderedLinkExtent(e10, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e12 = 
      toCompositeSecondEndOrderedLinkExtent(e11, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedRule, OTIUMLA_ownedRule_context, u.postcondition, u.precondition)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.postcondition, OTIUMLA_postcondition_behavior)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.precondition, OTIUMLA_precondition_behavior)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.redefinedBehavior, OTIUMLA_redefinedBehavior_behavior)
    val e26 =
      toReferenceLinkExtent(e25, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier, u.redefinedBehavior)
    val e27 =
      toReferenceLinkExtent(e26, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e28 =
      toReferenceLinkExtent(e27, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e28  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLGate[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLGate(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.message, OTIUMLA_message_messageEnd)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLGeneralOrdering[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLGeneralOrdering(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e2  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLGeneralization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLGeneralization(
            toolSpecific_id = u.toolSpecific_id,
            isSubstitutable = u.isSubstitutable))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e1  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLGeneralizationSet[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLGeneralizationSet(
            toolSpecific_id = u.toolSpecific_id,
            isCovering = u.isCovering,
            isDisjoint = u.isDisjoint,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalizationSet_generalization)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLImage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLImage(
            toolSpecific_id = u.toolSpecific_id,
            content = u.content,
            format = u.format,
            location = u.location))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e1  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInclude[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInclude(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e2  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInformationFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInformationFlow(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.conveyed, OTIUMLA_conveyed_conveyingFlow)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.informationSource, OTIUMLA_informationSource_informationFlow)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.informationTarget, OTIUMLA_informationTarget_informationFlow)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.realization, OTIUMLA_realization_abstraction_flow)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.realizingActivityEdge, OTIUMLA_realizingActivityEdge_informationFlow)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.realizingConnector, OTIUMLA_realizingConnector_informationFlow)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.realizingMessage, OTIUMLA_realizingMessage_informationFlow)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInformationItem[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInformationItem(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e12 =
      toReferenceLinkExtent(e11, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e13 =
      toReferenceLinkExtent(e12, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.represented, OTIUMLA_represented_representation)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e16  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInitialNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInitialNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInputPin(
            toolSpecific_id = u.toolSpecific_id,
            isControl = u.isControl,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            isOrdered = u.isOrdered,
            isUnique = u.isUnique,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.inState, OTIUMLA_inState_objectNode)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e9  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInstanceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInstanceSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.deployment, OTIUMLA_deployment_location)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.slot, OTIUMLA_slot_owningInstance)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.specification, OTIUMLA_specification_owningInstanceSpec)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.classifier, OTIUMLA_classifier_instanceSpecification)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInstanceValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInstanceValue(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInteraction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInteraction(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            isReentrant = u.isReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.action, OTIUMLA_action_interaction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.formalGate, OTIUMLA_formalGate_interaction)
    val e5 = 
      toCompositeSecondEndOrderedLinkExtent(e4, ud, u, u.fragment, OTIUMLA_fragment_enclosingInteraction)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.lifeline, OTIUMLA_lifeline_interaction)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.message, OTIUMLA_message_interaction)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e12 = 
      toCompositeSecondEndOrderedLinkExtent(e11, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e13 = 
      toCompositeFirstEndOrderedLinkExtent(e12, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e17 = 
      toCompositeSecondEndOrderedLinkExtent(e16, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e18 = 
      toCompositeSecondEndOrderedLinkExtent(e17, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.ownedRule, OTIUMLA_ownedRule_context, u.postcondition, u.precondition)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e23 = 
      toCompositeLinkExtent(e22, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e24 = 
      toCompositeLinkExtent(e23, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e25 = 
      toCompositeLinkExtent(e24, ud, u, u.postcondition, OTIUMLA_postcondition_behavior)
    val e26 = 
      toCompositeLinkExtent(e25, ud, u, u.precondition, OTIUMLA_precondition_behavior)
    val e27 = 
      toCompositeLinkExtent(e26, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e28 = 
      toCompositeLinkExtent(e27, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e29 =
      toReferenceLinkExtent(e28, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e30 =
      toReferenceLinkExtent(e29, ud, u, u.covered, OTIUMLA_covered_coveredBy)
    val e31 =
      toReferenceLinkExtent(e30, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e32 =
      toReferenceLinkExtent(e31, ud, u, u.redefinedBehavior, OTIUMLA_redefinedBehavior_behavior)
    val e33 =
      toReferenceLinkExtent(e32, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier, u.redefinedBehavior)
    val e34 =
      toReferenceLinkExtent(e33, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e35 =
      toReferenceLinkExtent(e34, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e35  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInteractionConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInteractionConstraint(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.maxint, OTIUMLA_maxint_interactionConstraint)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.minint, OTIUMLA_minint_interactionConstraint)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.specification, OTIUMLA_specification_owningConstraint)
    val e6 =
      toReferenceSecondEndOrderedLinkExtent(e5, ud, u, u.constrainedElement, OTIUMLA_constrainedElement_constraint)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInteractionOperand[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInteractionOperand(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e2 = 
      toCompositeFirstEndOrderedLinkExtent(e1, ud, u, u.fragment, OTIUMLA_fragment_enclosingOperand)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.guard, OTIUMLA_guard_interactionOperand)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.covered, OTIUMLA_covered_coveredBy)
    val result = e9  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInteractionUse[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInteractionUse(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.actualGate, OTIUMLA_actualGate_interactionUse)
    val e2 = 
      toCompositeSecondEndOrderedLinkExtent(e1, ud, u, u.argument, OTIUMLA_argument_interactionUse)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.returnValue, OTIUMLA_returnValue_interactionUse)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.covered, OTIUMLA_covered_coveredBy)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInterface[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInterface(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeSecondEndOrderedLinkExtent(e4, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_interface)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_interface)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e8 = 
      toCompositeSecondEndOrderedLinkExtent(e7, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_interface)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedReception, OTIUMLA_ownedReception_interface)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.protocol, OTIUMLA_protocol_interface)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier, u.redefinedInterface)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.redefinedInterface, OTIUMLA_redefinedInterface_interface)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e21  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInterfaceRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInterfaceRealization(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInterruptibleActivityRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInterruptibleActivityRegion(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.interruptingEdge, OTIUMLA_interruptingEdge_interrupts)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.node, OTIUMLA_inInterruptibleRegion_node)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLInterval(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLIntervalConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLIntervalConstraint(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.specification, OTIUMLA_specification_intervalConstraint)
    val e4 =
      toReferenceSecondEndOrderedLinkExtent(e3, ud, u, u.constrainedElement, OTIUMLA_constrainedElement_constraint)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLJoinNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLJoinNode(
            toolSpecific_id = u.toolSpecific_id,
            isCombineDuplicate = u.isCombineDuplicate,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.joinSpec, OTIUMLA_joinSpec_joinNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLLifeline[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLLifeline(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.selector, OTIUMLA_selector_lifeline)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.decomposedAs, OTIUMLA_decomposedAs_lifeline)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLLinkEndCreationData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLLinkEndCreationData(
            toolSpecific_id = u.toolSpecific_id,
            isReplaceAll = u.isReplaceAll))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.qualifier, OTIUMLA_qualifier_linkEndData)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.insertAt, OTIUMLA_insertAt_linkEndCreationData)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.value, OTIUMLA_value_linkEndData)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLLinkEndData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLLinkEndData(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.qualifier, OTIUMLA_qualifier_linkEndData)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.value, OTIUMLA_value_linkEndData)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLLinkEndDestructionData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLLinkEndDestructionData(
            toolSpecific_id = u.toolSpecific_id,
            isDestroyDuplicates = u.isDestroyDuplicates))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.qualifier, OTIUMLA_qualifier_linkEndData)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.destroyAt, OTIUMLA_destroyAt_linkEndDestructionData)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.value, OTIUMLA_value_linkEndData)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLLiteralBoolean[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLLiteralBoolean(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            value = u.value,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLLiteralInteger[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLLiteralInteger(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            value = u.value,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLLiteralNull[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLLiteralNull(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLLiteralReal[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLLiteralReal(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            value = u.value,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLLiteralString[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLLiteralString(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            value = u.value,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLLiteralUnlimitedNatural[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLLiteralUnlimitedNatural(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            value = u.value,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLLoopNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLLoopNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isTestedFirst = u.isTestedFirst,
            mustIsolate = u.mustIsolate,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.edge, OTIUMLA_edge_inStructuredNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.loopVariable, OTIUMLA_loopVariable_loopNode)
    val e7 = 
      toCompositeSecondEndOrderedLinkExtent(e6, ud, u, u.loopVariableInput, OTIUMLA_loopVariableInput_loopNode)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.node, OTIUMLA_node_inStructuredNode)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e13 = 
      toCompositeSecondEndOrderedLinkExtent(e12, ud, u, u.result, OTIUMLA_result_loopNode)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.variable, OTIUMLA_variable_scope)
    val e15 =
      toReferenceSecondEndOrderedLinkExtent(e14, ud, u, u.bodyOutput, OTIUMLA_bodyOutput_loopNode)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.bodyPart, OTIUMLA_bodyPart_loopNode)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.decider, OTIUMLA_decider_loopNode)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.setupPart, OTIUMLA_setupPart_loopNode)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.test, OTIUMLA_test_loopNode)
    val result = e22  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLManifestation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLManifestation(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLMergeNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLMergeNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLMessage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLMessage(
            toolSpecific_id = u.toolSpecific_id,
            messageSort = u.messageSort,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeSecondEndOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_message)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.receiveEvent, OTIUMLA_receiveEvent_endMessage)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.sendEvent, OTIUMLA_sendEvent_endMessage)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLMessageOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLMessageOccurrenceSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.message, OTIUMLA_message_messageEnd)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.toAfter, OTIUMLA_before_toAfter)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.toBefore, OTIUMLA_toBefore_after)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLModel[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLModel(
            toolSpecific_id = u.toolSpecific_id,
            URI = u.URI,
            name = u.name,
            viewpoint = u.viewpoint,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.packageMerge, OTIUMLA_packageMerge_receivingPackage)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.packagedElement, OTIUMLA_packagedElement_owningPackage)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.profileApplication, OTIUMLA_profileApplication_applyingPackage)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLNode(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e7 = 
      toCompositeSecondEndOrderedLinkExtent(e6, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nestedNode, OTIUMLA_nestedNode_node)
    val e9 = 
      toCompositeFirstEndOrderedLinkExtent(e8, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e13 = 
      toCompositeSecondEndOrderedLinkExtent(e12, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e25  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLObjectFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLObjectFlow(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isMulticast = u.isMulticast,
            isMultireceive = u.isMultireceive,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.guard, OTIUMLA_guard_activityEdge)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.weight, OTIUMLA_weight_activityEdge)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedEdge, OTIUMLA_redefinedEdge_activityEdge)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLOccurrenceSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.toAfter, OTIUMLA_before_toAfter)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.toBefore, OTIUMLA_toBefore_after)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLOpaqueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLOpaqueAction(
            toolSpecific_id = u.toolSpecific_id,
            body = u.body,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            language = u.language,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.inputValue, OTIUMLA_inputValue_opaqueAction)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.outputValue, OTIUMLA_outputValue_opaqueAction)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLOpaqueBehavior[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLOpaqueBehavior(
            toolSpecific_id = u.toolSpecific_id,
            body = u.body,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            isReentrant = u.isReentrant,
            language = u.language,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e7 = 
      toCompositeFirstEndOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e11 = 
      toCompositeSecondEndOrderedLinkExtent(e10, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e12 = 
      toCompositeSecondEndOrderedLinkExtent(e11, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedRule, OTIUMLA_ownedRule_context, u.postcondition, u.precondition)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.postcondition, OTIUMLA_postcondition_behavior)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.precondition, OTIUMLA_precondition_behavior)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.redefinedBehavior, OTIUMLA_redefinedBehavior_behavior)
    val e26 =
      toReferenceLinkExtent(e25, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier, u.redefinedBehavior)
    val e27 =
      toReferenceLinkExtent(e26, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e28 =
      toReferenceLinkExtent(e27, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e28  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLOpaqueExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLOpaqueExpression(
            toolSpecific_id = u.toolSpecific_id,
            body = u.body,
            language = u.language,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLOperation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLOperation(
            toolSpecific_id = u.toolSpecific_id,
            concurrency = u.concurrency,
            isAbstract = u.isAbstract,
            isLeaf = u.isLeaf,
            isQuery = u.isQuery,
            isStatic = u.isStatic,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.bodyCondition, OTIUMLA_bodyCondition_bodyContext)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e5 = 
      toCompositeFirstEndOrderedLinkExtent(e4, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_operation)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavioralFeature)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context, u.bodyCondition, u.postcondition, u.precondition)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.postcondition, OTIUMLA_postcondition_postContext)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.precondition, OTIUMLA_precondition_preContext)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e13 =
      toReferenceLinkExtent(e12, ud, u, u.method, OTIUMLA_method_specification)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.raisedException, OTIUMLA_raisedException_operation)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.redefinedOperation, OTIUMLA_redefinedOperation_operation)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.templateParameter, OTIUMLA_operation_templateParameter_parameteredElement)
    val result = e16  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLOperationTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLOperationTemplateParameter(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLOutputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLOutputPin(
            toolSpecific_id = u.toolSpecific_id,
            isControl = u.isControl,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            isOrdered = u.isOrdered,
            isUnique = u.isUnique,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.inState, OTIUMLA_inState_objectNode)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e9  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLPackage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLPackage(
            toolSpecific_id = u.toolSpecific_id,
            URI = u.URI,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.packageMerge, OTIUMLA_packageMerge_receivingPackage)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.packagedElement, OTIUMLA_packagedElement_owningPackage)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.profileApplication, OTIUMLA_profileApplication_applyingPackage)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLPackageImport[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLPackageImport(
            toolSpecific_id = u.toolSpecific_id,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e1  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLPackageMerge[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLPackageMerge(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e1  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLParameter(
            toolSpecific_id = u.toolSpecific_id,
            direction = u.direction,
            effect = u.effect,
            isException = u.isException,
            isOrdered = u.isOrdered,
            isStream = u.isStream,
            isUnique = u.isUnique,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.defaultValue, OTIUMLA_defaultValue_owningParameter)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.parameterSet, OTIUMLA_parameterSet_parameter)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLParameterSet[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLParameterSet(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.condition, OTIUMLA_condition_parameterSet)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLPartDecomposition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLPartDecomposition(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.actualGate, OTIUMLA_actualGate_interactionUse)
    val e2 = 
      toCompositeSecondEndOrderedLinkExtent(e1, ud, u, u.argument, OTIUMLA_argument_interactionUse)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.returnValue, OTIUMLA_returnValue_interactionUse)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.covered, OTIUMLA_covered_coveredBy)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLPort[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLPort(
            toolSpecific_id = u.toolSpecific_id,
            aggregation = u.aggregation,
            isBehavior = u.isBehavior,
            isConjugated = u.isConjugated,
            isDerived = u.isDerived,
            isDerivedUnion = u.isDerivedUnion,
            isID = u.isID,
            isLeaf = u.isLeaf,
            isOrdered = u.isOrdered,
            isReadOnly = u.isReadOnly,
            isService = u.isService,
            isStatic = u.isStatic,
            isUnique = u.isUnique,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.defaultValue, OTIUMLA_defaultValue_owningProperty)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.qualifier, OTIUMLA_qualifier_associationEnd)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.redefinedPort, OTIUMLA_redefinedPort_port)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedProperty, OTIUMLA_redefinedProperty_property, u.redefinedPort)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.subsettedProperty, OTIUMLA_subsettedProperty_property)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLPrimitiveType[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLPrimitiveType(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeFirstEndOrderedLinkExtent(e4, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_datatype)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeFirstEndOrderedLinkExtent(e6, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_datatype)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e17  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLProfile[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLProfile(
            toolSpecific_id = u.toolSpecific_id,
            URI = u.URI,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace, u.metaclassReference)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.metaclassReference, OTIUMLA_metaclassReference_profile)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.metamodelReference, OTIUMLA_metamodelReference_profile)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace, u.metamodelReference)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.packageMerge, OTIUMLA_packageMerge_receivingPackage)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packagedElement, OTIUMLA_packagedElement_owningPackage)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.profileApplication, OTIUMLA_profileApplication_applyingPackage)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e13 =
      toReferenceLinkExtent(e12, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e13  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLProfileApplication[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLProfileApplication(
            toolSpecific_id = u.toolSpecific_id,
            isStrict = u.isStrict))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e1  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLProperty[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLProperty(
            toolSpecific_id = u.toolSpecific_id,
            aggregation = u.aggregation,
            isDerived = u.isDerived,
            isDerivedUnion = u.isDerivedUnion,
            isID = u.isID,
            isLeaf = u.isLeaf,
            isOrdered = u.isOrdered,
            isReadOnly = u.isReadOnly,
            isStatic = u.isStatic,
            isUnique = u.isUnique,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.defaultValue, OTIUMLA_defaultValue_owningProperty)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.qualifier, OTIUMLA_qualifier_associationEnd)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.redefinedProperty, OTIUMLA_redefinedProperty_property)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.subsettedProperty, OTIUMLA_subsettedProperty_property)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLProtocolConformance[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLProtocolConformance(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e1  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLProtocolStateMachine[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLProtocolStateMachine(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            isReentrant = u.isReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.conformance, OTIUMLA_conformance_specificMachine)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.connectionPoint, OTIUMLA_connectionPoint_stateMachine)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e8 = 
      toCompositeSecondEndOrderedLinkExtent(e7, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e9 = 
      toCompositeFirstEndOrderedLinkExtent(e8, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e13 = 
      toCompositeSecondEndOrderedLinkExtent(e12, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e14 = 
      toCompositeSecondEndOrderedLinkExtent(e13, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedRule, OTIUMLA_ownedRule_context, u.postcondition, u.precondition)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.postcondition, OTIUMLA_postcondition_behavior)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.precondition, OTIUMLA_precondition_behavior)
    val e23 = 
      toCompositeLinkExtent(e22, ud, u, u.region, OTIUMLA_region_stateMachine)
    val e24 = 
      toCompositeLinkExtent(e23, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e25 = 
      toCompositeLinkExtent(e24, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e26 =
      toReferenceLinkExtent(e25, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e27 =
      toReferenceLinkExtent(e26, ud, u, u.extendedStateMachine, OTIUMLA_extendedStateMachine_stateMachine)
    val e28 =
      toReferenceLinkExtent(e27, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e29 =
      toReferenceLinkExtent(e28, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier, u.extendedStateMachine)
    val e30 =
      toReferenceLinkExtent(e29, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e31 =
      toReferenceLinkExtent(e30, ud, u, u.submachineState, OTIUMLA_submachineState_submachine)
    val e32 =
      toReferenceLinkExtent(e31, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e32  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLProtocolTransition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLProtocolTransition(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            kind = u.kind,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.effect, OTIUMLA_effect_transition)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.guard, OTIUMLA_guard_transition, u.preCondition)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedRule, OTIUMLA_ownedRule_context, u.guard, u.postCondition, u.preCondition)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.postCondition, OTIUMLA_postCondition_owningTransition)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.preCondition, OTIUMLA_preCondition_protocolTransition)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.trigger, OTIUMLA_trigger_transition)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLPseudostate[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLPseudostate(
            toolSpecific_id = u.toolSpecific_id,
            kind = u.kind,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e2  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLQualifierValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLQualifierValue(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 =
      toReferenceLinkExtent(e1, ud, u, u.value, OTIUMLA_value_qualifierValue)
    val result = e2  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLRaiseExceptionAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLRaiseExceptionAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.exception, OTIUMLA_exception_raiseExceptionAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e9  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReadExtentAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReadExtentAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.result, OTIUMLA_result_readExtentAction)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.classifier, OTIUMLA_classifier_readExtentAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReadIsClassifiedObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReadIsClassifiedObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isDirect = u.isDirect,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_readIsClassifiedObjectAction)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_readIsClassifiedObjectAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReadLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReadLinkAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.endData, OTIUMLA_endData_linkAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.inputValue, OTIUMLA_inputValue_linkAction)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.result, OTIUMLA_result_readLinkAction)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReadLinkObjectEndAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReadLinkObjectEndAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_readLinkObjectEndAction)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_readLinkObjectEndAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.end, OTIUMLA_end_readLinkObjectEndAction)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReadLinkObjectEndQualifierAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReadLinkObjectEndQualifierAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_readLinkObjectEndQualifierAction)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_readLinkObjectEndQualifierAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.qualifier, OTIUMLA_qualifier_readLinkObjectEndQualifierAction)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReadSelfAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReadSelfAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.result, OTIUMLA_result_readSelfAction)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e9  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReadStructuralFeatureAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReadStructuralFeatureAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_structuralFeatureAction)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_readStructuralFeatureAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReadVariableAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReadVariableAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.result, OTIUMLA_result_readVariableAction)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e9  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLRealization(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReception[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReception(
            toolSpecific_id = u.toolSpecific_id,
            concurrency = u.concurrency,
            isAbstract = u.isAbstract,
            isLeaf = u.isLeaf,
            isStatic = u.isStatic,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeSecondEndOrderedLinkExtent(e3, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_ownerFormalParam)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavioralFeature)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.method, OTIUMLA_method_specification)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.raisedException, OTIUMLA_raisedException_behavioralFeature)
    val result = e9  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReclassifyObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReclassifyObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isReplaceAll = u.isReplaceAll,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_reclassifyObjectAction)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.newClassifier, OTIUMLA_newClassifier_reclassifyObjectAction)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.oldClassifier, OTIUMLA_oldClassifier_reclassifyObjectAction)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLRedefinableTemplateSignature[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLRedefinableTemplateSignature(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeFirstEndOrderedLinkExtent(e2, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_signature)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.extendedSignature, OTIUMLA_extendedSignature_redefinableTemplateSignature)
    val e5 =
      toReferenceSecondEndOrderedLinkExtent(e4, ud, u, u.parameter, OTIUMLA_parameter_templateSignature)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReduceAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReduceAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isOrdered = u.isOrdered,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collection, OTIUMLA_collection_reduceAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_reduceAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLRegion(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.subvertex, OTIUMLA_subvertex_container)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.transition, OTIUMLA_transition_container)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLRemoveStructuralFeatureValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLRemoveStructuralFeatureValueAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isRemoveDuplicates = u.isRemoveDuplicates,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_structuralFeatureAction)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.removeAt, OTIUMLA_removeAt_removeStructuralFeatureValueAction)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.result, OTIUMLA_result_writeStructuralFeatureAction)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.value, OTIUMLA_value_writeStructuralFeatureAction)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e12 =
      toReferenceLinkExtent(e11, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e12  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLRemoveVariableValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLRemoveVariableValueAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isRemoveDuplicates = u.isRemoveDuplicates,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.removeAt, OTIUMLA_removeAt_removeVariableValueAction)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.value, OTIUMLA_value_writeVariableAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLReplyAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLReplyAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeSecondEndOrderedLinkExtent(e5, ud, u, u.replyValue, OTIUMLA_replyValue_replyAction)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.returnInformation, OTIUMLA_returnInformation_replyAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.replyToCall, OTIUMLA_replyToCall_replyAction)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLSendObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLSendObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.request, OTIUMLA_request_sendObjectAction)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.target, OTIUMLA_target_sendObjectAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLSendSignalAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLSendSignalAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeSecondEndOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_invocationAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.target, OTIUMLA_target_sendSignalAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLSequenceNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLSequenceNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            mustIsolate = u.mustIsolate,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.edge, OTIUMLA_edge_inStructuredNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeSecondEndOrderedLinkExtent(e2, ud, u, u.executableNode, OTIUMLA_executableNode_sequenceNode)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.structuredNodeOutput, OTIUMLA_structuredNodeOutput_structuredActivityNode)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.variable, OTIUMLA_variable_scope)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e16  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLSignal[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLSignal(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeSecondEndOrderedLinkExtent(e4, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_owningSignal)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e13 =
      toReferenceLinkExtent(e12, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e16  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLSignalEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLSignalEvent(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLSlot[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLSlot(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 = 
      toCompositeSecondEndOrderedLinkExtent(e1, ud, u, u.value, OTIUMLA_value_owningSlot)
    val result = e2  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLStartClassifierBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLStartClassifierBehaviorAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_startClassifierBehaviorAction)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e9  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLStartObjectBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLStartObjectBehaviorAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isSynchronous = u.isSynchronous,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeSecondEndOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_invocationAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u._object, OTIUMLA_object_startObjectBehaviorAction)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e8 = 
      toCompositeSecondEndOrderedLinkExtent(e7, ud, u, u.result, OTIUMLA_result_callAction)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLState[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLState(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.connection, OTIUMLA_connection_state)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.connectionPoint, OTIUMLA_connectionPoint_state)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.deferrableTrigger, OTIUMLA_deferrableTrigger_state)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.doActivity, OTIUMLA_doActivity_state)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.entry, OTIUMLA_entry_state)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.exit, OTIUMLA_exit_state)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context, u.stateInvariant)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.region, OTIUMLA_region_state)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.stateInvariant, OTIUMLA_stateInvariant_owningState)
    val result = e13  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLStateInvariant[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLStateInvariant(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.invariant, OTIUMLA_invariant_stateInvariant)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLStateMachine[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLStateMachine(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            isReentrant = u.isReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.connectionPoint, OTIUMLA_connectionPoint_stateMachine)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e7 = 
      toCompositeSecondEndOrderedLinkExtent(e6, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e8 = 
      toCompositeFirstEndOrderedLinkExtent(e7, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e12 = 
      toCompositeSecondEndOrderedLinkExtent(e11, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e13 = 
      toCompositeSecondEndOrderedLinkExtent(e12, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedRule, OTIUMLA_ownedRule_context, u.postcondition, u.precondition)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.postcondition, OTIUMLA_postcondition_behavior)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.precondition, OTIUMLA_precondition_behavior)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.region, OTIUMLA_region_stateMachine)
    val e23 = 
      toCompositeLinkExtent(e22, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e24 = 
      toCompositeLinkExtent(e23, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e26 =
      toReferenceLinkExtent(e25, ud, u, u.extendedStateMachine, OTIUMLA_extendedStateMachine_stateMachine)
    val e27 =
      toReferenceLinkExtent(e26, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e28 =
      toReferenceLinkExtent(e27, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier, u.extendedStateMachine)
    val e29 =
      toReferenceLinkExtent(e28, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e30 =
      toReferenceLinkExtent(e29, ud, u, u.submachineState, OTIUMLA_submachineState_submachine)
    val e31 =
      toReferenceLinkExtent(e30, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e31  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLStereotype[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLStereotype(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.icon, OTIUMLA_icon_stereotype)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e7 = 
      toCompositeSecondEndOrderedLinkExtent(e6, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    val e8 = 
      toCompositeFirstEndOrderedLinkExtent(e7, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    val e12 = 
      toCompositeSecondEndOrderedLinkExtent(e11, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedReception, OTIUMLA_ownedReception_class)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e24  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLStringExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLStringExpression(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            symbol = u.symbol,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeSecondEndOrderedLinkExtent(e1, ud, u, u.operand, OTIUMLA_operand_expression)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template)
    val e5 = 
      toCompositeSecondEndOrderedLinkExtent(e4, ud, u, u.subExpression, OTIUMLA_subExpression_owningExpression)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e7  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLStructuredActivityNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLStructuredActivityNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            mustIsolate = u.mustIsolate,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.edge, OTIUMLA_edge_inStructuredNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.node, OTIUMLA_node_inStructuredNode)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.structuredNodeOutput, OTIUMLA_structuredNodeOutput_structuredActivityNode)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.variable, OTIUMLA_variable_scope)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e16  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLSubstitution[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLSubstitution(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e6  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTemplateBinding[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTemplateBinding(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.parameterSubstitution, OTIUMLA_parameterSubstitution_templateBinding)
    val result = e2  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTemplateParameter(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTemplateParameterSubstitution[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTemplateParameterSubstitution(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedActual, OTIUMLA_ownedActual_owningTemplateParameterSubstitution)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val result = e2  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTemplateSignature[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTemplateSignature(
            toolSpecific_id = u.toolSpecific_id))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e2 = 
      toCompositeFirstEndOrderedLinkExtent(e1, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_signature)
    val e3 =
      toReferenceSecondEndOrderedLinkExtent(e2, ud, u, u.parameter, OTIUMLA_parameter_templateSignature)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTestIdentityAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTestIdentityAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.first, OTIUMLA_first_testIdentityAction)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_testIdentityAction)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.second, OTIUMLA_second_testIdentityAction)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e11  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTimeConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTimeConstraint(
            toolSpecific_id = u.toolSpecific_id,
            firstEvent = u.firstEvent,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.specification, OTIUMLA_specification_timeConstraint)
    val e4 =
      toReferenceSecondEndOrderedLinkExtent(e3, ud, u, u.constrainedElement, OTIUMLA_constrainedElement_constraint)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTimeEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTimeEvent(
            toolSpecific_id = u.toolSpecific_id,
            isRelative = u.isRelative,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.when, OTIUMLA_when_timeEvent)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e4  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTimeExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTimeExpression(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.expr, OTIUMLA_expr_timeExpression)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.observation, OTIUMLA_observation_timeExpression)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTimeInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTimeInterval(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTimeObservation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTimeObservation(
            toolSpecific_id = u.toolSpecific_id,
            firstEvent = u.firstEvent,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTransition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTransition(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            kind = u.kind,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.effect, OTIUMLA_effect_transition)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.guard, OTIUMLA_guard_transition)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedRule, OTIUMLA_ownedRule_context, u.guard)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.trigger, OTIUMLA_trigger_transition)
    val result = e8  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLTrigger[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLTrigger(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.port, OTIUMLA_port_trigger)
    val result = e3  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLUnmarshallAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLUnmarshallAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_unmarshallAction)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e7 = 
      toCompositeSecondEndOrderedLinkExtent(e6, ud, u, u.result, OTIUMLA_result_unmarshallAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLUsage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLUsage(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.client, OTIUMLA_clientDependency_client)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter)
    val result = e5  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLUseCase[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLUseCase(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.extend, OTIUMLA_extend_extension)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.extensionPoint, OTIUMLA_extensionPoint_useCase)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.generalization, OTIUMLA_generalization_specific)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.include, OTIUMLA_include_includingCase)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedRule, OTIUMLA_ownedRule_context)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.representation, OTIUMLA_representation_classifier)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.subject, OTIUMLA_subject_useCase)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement)
    val result = e22  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLValuePin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLValuePin(
            toolSpecific_id = u.toolSpecific_id,
            isControl = u.isControl,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            isOrdered = u.isOrdered,
            isUnique = u.isUnique,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.value, OTIUMLA_value_valuePin)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.inState, OTIUMLA_inState_objectNode)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLValueSpecificationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLValueSpecificationAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.result, OTIUMLA_result_valueSpecificationAction)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.value, OTIUMLA_value_valueSpecificationAction)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode)
    val result = e10  
    result
  }

  def toOTI
  (extent: OTIDocumentExtent,
   u: UMLVariable[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          OTIMOFElement.OTIUMLVariable(
            toolSpecific_id = u.toolSpecific_id,
            isOrdered = u.isOrdered,
            isUnique = u.isUnique,
            name = u.name,
            visibility = u.visibility))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement)
    val result = e5  
    result
  }
}
