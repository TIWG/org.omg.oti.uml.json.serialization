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

import scala.language.implicitConversions
import scala.collection.immutable._
import scala.{Boolean, Double, Function1, Int, Option, Some}
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

  def toElementLocation
  ( context: Document[Uml],
    element: UMLElement[Uml],
    elementDocument: Document[Uml] )
  : ElementLocation
  = if (context == elementDocument)
    ElementLocation_ToolSpecific_ID(element.toolSpecific_id)
  else
    ElementLocation_ToolSpecific_ID_OTI_URL(element.toolSpecific_id, elementDocument.info.documentURL)

  def allExcluded[V <: UMLElement[Uml]]
  (vExcludes: scala.collection.Seq[scala.collection.Iterable[V]])
  : Set[V]
  = ( Set.empty[V] /: vExcludes ) { _ ++ _ }

  def toCompositeLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U, 
   v : V, 
   ctor: (ElementLocation, ElementLocation) => OTIMOFCompositeLink)
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(v).fold[OTIDocumentExtent](extent) { vd =>
    extent.copy(
      compositeLinkExtent =
        extent.compositeLinkExtent :+
          ctor(
            ElementLocation_ToolSpecific_ID(u.toolSpecific_id),
            toElementLocation(ud, v, vd)))
  }

  def toCompositeLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : scala.collection.Iterable[V],
   ctor: (ElementLocation, ElementLocation) => OTIMOFCompositeLink,
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

  def toCompositeOrderedLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : V,
   vIndex: Int,
   ctor: (ElementLocation, ElementLocation, Int) => OTIMOFCompositeOrderedLink)
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(v).fold[OTIDocumentExtent](extent) { vd =>
    extent.copy(
      compositeOrderedLinkExtent =
        extent.compositeOrderedLinkExtent :+
          ctor(
            ElementLocation_ToolSpecific_ID(u.toolSpecific_id),
            toElementLocation(ud, v, vd),
            vIndex))
  }

  def toCompositeOrderedLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : scala.collection.Iterable[V],
   ctor: (ElementLocation, ElementLocation, Int) => OTIMOFCompositeOrderedLink,
   vExcludes: scala.collection.Iterable[V]*)
  : OTIDocumentExtent
  = {
    val excluded = allExcluded[V](vExcludes)

    ( extent /: v.zipWithIndex ) { case (ei, (uv, uvIndex)) =>
      if (excluded.contains(uv))
        ei
      else
        toCompositeOrderedLinkExtent(extent, ud, u, uv, uvIndex, ctor)
    }
  }

  // ============

  def toReferenceLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : V,
   ctor: (ElementLocation, ElementLocation) => OTIMOFReferenceLink)
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(v).fold[OTIDocumentExtent](extent) { vd =>
    extent.copy(
      referenceLinkExtent =
        extent.referenceLinkExtent :+
          ctor(
            ElementLocation_ToolSpecific_ID(u.toolSpecific_id),
            toElementLocation(ud, v, vd)))
  }

  def toReferenceLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : scala.collection.Iterable[V],
   ctor: (ElementLocation, ElementLocation) => OTIMOFReferenceLink,
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

  def toReferenceOrderedLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : V,
   vIndex: Int,
   ctor: (ElementLocation, ElementLocation, Int) => OTIMOFReferenceOrderedLink)
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(v).fold[OTIDocumentExtent](extent) { vd =>
    extent.copy(
      referenceOrderedLinkExtent =
        extent.referenceOrderedLinkExtent :+
          ctor(
            ElementLocation_ToolSpecific_ID(u.toolSpecific_id),
            toElementLocation(ud, v, vd),
            vIndex))
  }

  def toReferenceOrderedLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (extent: OTIDocumentExtent,
   ud: Document[Uml],
   u : U,
   v : Seq[V],
   ctor: (ElementLocation, ElementLocation, Int) => OTIMOFReferenceOrderedLink,
   vExcludes: scala.collection.Iterable[V]*)
  : OTIDocumentExtent
  = {
    val excluded = allExcluded[V](vExcludes)

    ( extent /: v.zipWithIndex ) { case (ei, (uv, uvIndex)) =>
      if (excluded.contains(uv))
        ei
      else
        toReferenceOrderedLinkExtent(extent, ud, u, uv, uvIndex, ctor)
    }
  }

  // ============

  implicit def optionToOTI[U,V]
  (value: Option[U])
  (implicit u2v: U => V)
  : Option[V]
  = value.map(u2v)

  val otiJsonElementHelper = OTIJsonElementHelper(odsa.otiAdapter, Some(odsa.ds))

  // <!-- End of user code additions -->

  def addToOTIDocumentExtent
  (extent: OTIDocumentExtent,
   u: UMLElement[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = u match {
    case uu: UMLAcceptCallAction[Uml] => addOTIUMLAcceptCallAction(extent, uu)
    case uu: UMLActionExecutionSpecification[Uml] => addOTIUMLActionExecutionSpecification(extent, uu)
    case uu: UMLActionInputPin[Uml] => addOTIUMLActionInputPin(extent, uu)
    case uu: UMLActivity[Uml] => addOTIUMLActivity(extent, uu)
    case uu: UMLActivityFinalNode[Uml] => addOTIUMLActivityFinalNode(extent, uu)
    case uu: UMLActivityParameterNode[Uml] => addOTIUMLActivityParameterNode(extent, uu)
    case uu: UMLActivityPartition[Uml] => addOTIUMLActivityPartition(extent, uu)
    case uu: UMLActor[Uml] => addOTIUMLActor(extent, uu)
    case uu: UMLAddStructuralFeatureValueAction[Uml] => addOTIUMLAddStructuralFeatureValueAction(extent, uu)
    case uu: UMLAddVariableValueAction[Uml] => addOTIUMLAddVariableValueAction(extent, uu)
    case uu: UMLAnyReceiveEvent[Uml] => addOTIUMLAnyReceiveEvent(extent, uu)
    case uu: UMLAssociationClass[Uml] => addOTIUMLAssociationClass(extent, uu)
    case uu: UMLBehaviorExecutionSpecification[Uml] => addOTIUMLBehaviorExecutionSpecification(extent, uu)
    case uu: UMLBroadcastSignalAction[Uml] => addOTIUMLBroadcastSignalAction(extent, uu)
    case uu: UMLCallBehaviorAction[Uml] => addOTIUMLCallBehaviorAction(extent, uu)
    case uu: UMLCallEvent[Uml] => addOTIUMLCallEvent(extent, uu)
    case uu: UMLCallOperationAction[Uml] => addOTIUMLCallOperationAction(extent, uu)
    case uu: UMLChangeEvent[Uml] => addOTIUMLChangeEvent(extent, uu)
    case uu: UMLClassifierTemplateParameter[Uml] => addOTIUMLClassifierTemplateParameter(extent, uu)
    case uu: UMLClause[Uml] => addOTIUMLClause(extent, uu)
    case uu: UMLClearAssociationAction[Uml] => addOTIUMLClearAssociationAction(extent, uu)
    case uu: UMLClearStructuralFeatureAction[Uml] => addOTIUMLClearStructuralFeatureAction(extent, uu)
    case uu: UMLClearVariableAction[Uml] => addOTIUMLClearVariableAction(extent, uu)
    case uu: UMLCollaboration[Uml] => addOTIUMLCollaboration(extent, uu)
    case uu: UMLCollaborationUse[Uml] => addOTIUMLCollaborationUse(extent, uu)
    case uu: UMLComment[Uml] => addOTIUMLComment(extent, uu)
    case uu: UMLCommunicationPath[Uml] => addOTIUMLCommunicationPath(extent, uu)
    case uu: UMLComponent[Uml] => addOTIUMLComponent(extent, uu)
    case uu: UMLComponentRealization[Uml] => addOTIUMLComponentRealization(extent, uu)
    case uu: UMLConditionalNode[Uml] => addOTIUMLConditionalNode(extent, uu)
    case uu: UMLConnectableElementTemplateParameter[Uml] => addOTIUMLConnectableElementTemplateParameter(extent, uu)
    case uu: UMLConnectionPointReference[Uml] => addOTIUMLConnectionPointReference(extent, uu)
    case uu: UMLConnector[Uml] => addOTIUMLConnector(extent, uu)
    case uu: UMLConnectorEnd[Uml] => addOTIUMLConnectorEnd(extent, uu)
    case uu: UMLConsiderIgnoreFragment[Uml] => addOTIUMLConsiderIgnoreFragment(extent, uu)
    case uu: UMLContinuation[Uml] => addOTIUMLContinuation(extent, uu)
    case uu: UMLControlFlow[Uml] => addOTIUMLControlFlow(extent, uu)
    case uu: UMLCreateLinkObjectAction[Uml] => addOTIUMLCreateLinkObjectAction(extent, uu)
    case uu: UMLCreateObjectAction[Uml] => addOTIUMLCreateObjectAction(extent, uu)
    case uu: UMLDataStoreNode[Uml] => addOTIUMLDataStoreNode(extent, uu)
    case uu: UMLDecisionNode[Uml] => addOTIUMLDecisionNode(extent, uu)
    case uu: UMLDeployment[Uml] => addOTIUMLDeployment(extent, uu)
    case uu: UMLDeploymentSpecification[Uml] => addOTIUMLDeploymentSpecification(extent, uu)
    case uu: UMLDestroyLinkAction[Uml] => addOTIUMLDestroyLinkAction(extent, uu)
    case uu: UMLDestroyObjectAction[Uml] => addOTIUMLDestroyObjectAction(extent, uu)
    case uu: UMLDestructionOccurrenceSpecification[Uml] => addOTIUMLDestructionOccurrenceSpecification(extent, uu)
    case uu: UMLDevice[Uml] => addOTIUMLDevice(extent, uu)
    case uu: UMLDuration[Uml] => addOTIUMLDuration(extent, uu)
    case uu: UMLDurationConstraint[Uml] => addOTIUMLDurationConstraint(extent, uu)
    case uu: UMLDurationInterval[Uml] => addOTIUMLDurationInterval(extent, uu)
    case uu: UMLDurationObservation[Uml] => addOTIUMLDurationObservation(extent, uu)
    case uu: UMLElementImport[Uml] => addOTIUMLElementImport(extent, uu)
    case uu: UMLElementValue[Uml] => addOTIUMLElementValue(extent, uu)
    case uu: UMLEnumeration[Uml] => addOTIUMLEnumeration(extent, uu)
    case uu: UMLEnumerationLiteral[Uml] => addOTIUMLEnumerationLiteral(extent, uu)
    case uu: UMLExceptionHandler[Uml] => addOTIUMLExceptionHandler(extent, uu)
    case uu: UMLExecutionEnvironment[Uml] => addOTIUMLExecutionEnvironment(extent, uu)
    case uu: UMLExecutionOccurrenceSpecification[Uml] => addOTIUMLExecutionOccurrenceSpecification(extent, uu)
    case uu: UMLExpansionNode[Uml] => addOTIUMLExpansionNode(extent, uu)
    case uu: UMLExpansionRegion[Uml] => addOTIUMLExpansionRegion(extent, uu)
    case uu: UMLExtend[Uml] => addOTIUMLExtend(extent, uu)
    case uu: UMLExtension[Uml] => addOTIUMLExtension(extent, uu)
    case uu: UMLExtensionEnd[Uml] => addOTIUMLExtensionEnd(extent, uu)
    case uu: UMLExtensionPoint[Uml] => addOTIUMLExtensionPoint(extent, uu)
    case uu: UMLFinalState[Uml] => addOTIUMLFinalState(extent, uu)
    case uu: UMLFlowFinalNode[Uml] => addOTIUMLFlowFinalNode(extent, uu)
    case uu: UMLForkNode[Uml] => addOTIUMLForkNode(extent, uu)
    case uu: UMLFunctionBehavior[Uml] => addOTIUMLFunctionBehavior(extent, uu)
    case uu: UMLGate[Uml] => addOTIUMLGate(extent, uu)
    case uu: UMLGeneralOrdering[Uml] => addOTIUMLGeneralOrdering(extent, uu)
    case uu: UMLGeneralization[Uml] => addOTIUMLGeneralization(extent, uu)
    case uu: UMLGeneralizationSet[Uml] => addOTIUMLGeneralizationSet(extent, uu)
    case uu: UMLImage[Uml] => addOTIUMLImage(extent, uu)
    case uu: UMLInclude[Uml] => addOTIUMLInclude(extent, uu)
    case uu: UMLInformationFlow[Uml] => addOTIUMLInformationFlow(extent, uu)
    case uu: UMLInformationItem[Uml] => addOTIUMLInformationItem(extent, uu)
    case uu: UMLInitialNode[Uml] => addOTIUMLInitialNode(extent, uu)
    case uu: UMLInstanceValue[Uml] => addOTIUMLInstanceValue(extent, uu)
    case uu: UMLInteraction[Uml] => addOTIUMLInteraction(extent, uu)
    case uu: UMLInteractionConstraint[Uml] => addOTIUMLInteractionConstraint(extent, uu)
    case uu: UMLInteractionOperand[Uml] => addOTIUMLInteractionOperand(extent, uu)
    case uu: UMLInterface[Uml] => addOTIUMLInterface(extent, uu)
    case uu: UMLInterfaceRealization[Uml] => addOTIUMLInterfaceRealization(extent, uu)
    case uu: UMLInterruptibleActivityRegion[Uml] => addOTIUMLInterruptibleActivityRegion(extent, uu)
    case uu: UMLJoinNode[Uml] => addOTIUMLJoinNode(extent, uu)
    case uu: UMLLifeline[Uml] => addOTIUMLLifeline(extent, uu)
    case uu: UMLLinkEndCreationData[Uml] => addOTIUMLLinkEndCreationData(extent, uu)
    case uu: UMLLinkEndDestructionData[Uml] => addOTIUMLLinkEndDestructionData(extent, uu)
    case uu: UMLLiteralBoolean[Uml] => addOTIUMLLiteralBoolean(extent, uu)
    case uu: UMLLiteralInteger[Uml] => addOTIUMLLiteralInteger(extent, uu)
    case uu: UMLLiteralNull[Uml] => addOTIUMLLiteralNull(extent, uu)
    case uu: UMLLiteralReal[Uml] => addOTIUMLLiteralReal(extent, uu)
    case uu: UMLLiteralString[Uml] => addOTIUMLLiteralString(extent, uu)
    case uu: UMLLiteralUnlimitedNatural[Uml] => addOTIUMLLiteralUnlimitedNatural(extent, uu)
    case uu: UMLLoopNode[Uml] => addOTIUMLLoopNode(extent, uu)
    case uu: UMLManifestation[Uml] => addOTIUMLManifestation(extent, uu)
    case uu: UMLMergeNode[Uml] => addOTIUMLMergeNode(extent, uu)
    case uu: UMLMessage[Uml] => addOTIUMLMessage(extent, uu)
    case uu: UMLModel[Uml] => addOTIUMLModel(extent, uu)
    case uu: UMLObjectFlow[Uml] => addOTIUMLObjectFlow(extent, uu)
    case uu: UMLOpaqueAction[Uml] => addOTIUMLOpaqueAction(extent, uu)
    case uu: UMLOpaqueExpression[Uml] => addOTIUMLOpaqueExpression(extent, uu)
    case uu: UMLOperation[Uml] => addOTIUMLOperation(extent, uu)
    case uu: UMLOperationTemplateParameter[Uml] => addOTIUMLOperationTemplateParameter(extent, uu)
    case uu: UMLOutputPin[Uml] => addOTIUMLOutputPin(extent, uu)
    case uu: UMLPackageImport[Uml] => addOTIUMLPackageImport(extent, uu)
    case uu: UMLPackageMerge[Uml] => addOTIUMLPackageMerge(extent, uu)
    case uu: UMLParameter[Uml] => addOTIUMLParameter(extent, uu)
    case uu: UMLParameterSet[Uml] => addOTIUMLParameterSet(extent, uu)
    case uu: UMLPartDecomposition[Uml] => addOTIUMLPartDecomposition(extent, uu)
    case uu: UMLPort[Uml] => addOTIUMLPort(extent, uu)
    case uu: UMLPrimitiveType[Uml] => addOTIUMLPrimitiveType(extent, uu)
    case uu: UMLProfile[Uml] => addOTIUMLProfile(extent, uu)
    case uu: UMLProfileApplication[Uml] => addOTIUMLProfileApplication(extent, uu)
    case uu: UMLProtocolConformance[Uml] => addOTIUMLProtocolConformance(extent, uu)
    case uu: UMLProtocolStateMachine[Uml] => addOTIUMLProtocolStateMachine(extent, uu)
    case uu: UMLProtocolTransition[Uml] => addOTIUMLProtocolTransition(extent, uu)
    case uu: UMLPseudostate[Uml] => addOTIUMLPseudostate(extent, uu)
    case uu: UMLQualifierValue[Uml] => addOTIUMLQualifierValue(extent, uu)
    case uu: UMLRaiseExceptionAction[Uml] => addOTIUMLRaiseExceptionAction(extent, uu)
    case uu: UMLReadExtentAction[Uml] => addOTIUMLReadExtentAction(extent, uu)
    case uu: UMLReadIsClassifiedObjectAction[Uml] => addOTIUMLReadIsClassifiedObjectAction(extent, uu)
    case uu: UMLReadLinkAction[Uml] => addOTIUMLReadLinkAction(extent, uu)
    case uu: UMLReadLinkObjectEndAction[Uml] => addOTIUMLReadLinkObjectEndAction(extent, uu)
    case uu: UMLReadLinkObjectEndQualifierAction[Uml] => addOTIUMLReadLinkObjectEndQualifierAction(extent, uu)
    case uu: UMLReadSelfAction[Uml] => addOTIUMLReadSelfAction(extent, uu)
    case uu: UMLReadStructuralFeatureAction[Uml] => addOTIUMLReadStructuralFeatureAction(extent, uu)
    case uu: UMLReadVariableAction[Uml] => addOTIUMLReadVariableAction(extent, uu)
    case uu: UMLReception[Uml] => addOTIUMLReception(extent, uu)
    case uu: UMLReclassifyObjectAction[Uml] => addOTIUMLReclassifyObjectAction(extent, uu)
    case uu: UMLRedefinableTemplateSignature[Uml] => addOTIUMLRedefinableTemplateSignature(extent, uu)
    case uu: UMLReduceAction[Uml] => addOTIUMLReduceAction(extent, uu)
    case uu: UMLRegion[Uml] => addOTIUMLRegion(extent, uu)
    case uu: UMLRemoveStructuralFeatureValueAction[Uml] => addOTIUMLRemoveStructuralFeatureValueAction(extent, uu)
    case uu: UMLRemoveVariableValueAction[Uml] => addOTIUMLRemoveVariableValueAction(extent, uu)
    case uu: UMLReplyAction[Uml] => addOTIUMLReplyAction(extent, uu)
    case uu: UMLSendObjectAction[Uml] => addOTIUMLSendObjectAction(extent, uu)
    case uu: UMLSendSignalAction[Uml] => addOTIUMLSendSignalAction(extent, uu)
    case uu: UMLSequenceNode[Uml] => addOTIUMLSequenceNode(extent, uu)
    case uu: UMLSignal[Uml] => addOTIUMLSignal(extent, uu)
    case uu: UMLSignalEvent[Uml] => addOTIUMLSignalEvent(extent, uu)
    case uu: UMLSlot[Uml] => addOTIUMLSlot(extent, uu)
    case uu: UMLStartClassifierBehaviorAction[Uml] => addOTIUMLStartClassifierBehaviorAction(extent, uu)
    case uu: UMLStartObjectBehaviorAction[Uml] => addOTIUMLStartObjectBehaviorAction(extent, uu)
    case uu: UMLStateInvariant[Uml] => addOTIUMLStateInvariant(extent, uu)
    case uu: UMLStereotype[Uml] => addOTIUMLStereotype(extent, uu)
    case uu: UMLStringExpression[Uml] => addOTIUMLStringExpression(extent, uu)
    case uu: UMLSubstitution[Uml] => addOTIUMLSubstitution(extent, uu)
    case uu: UMLTemplateBinding[Uml] => addOTIUMLTemplateBinding(extent, uu)
    case uu: UMLTemplateParameterSubstitution[Uml] => addOTIUMLTemplateParameterSubstitution(extent, uu)
    case uu: UMLTestIdentityAction[Uml] => addOTIUMLTestIdentityAction(extent, uu)
    case uu: UMLTimeConstraint[Uml] => addOTIUMLTimeConstraint(extent, uu)
    case uu: UMLTimeEvent[Uml] => addOTIUMLTimeEvent(extent, uu)
    case uu: UMLTimeExpression[Uml] => addOTIUMLTimeExpression(extent, uu)
    case uu: UMLTimeInterval[Uml] => addOTIUMLTimeInterval(extent, uu)
    case uu: UMLTimeObservation[Uml] => addOTIUMLTimeObservation(extent, uu)
    case uu: UMLTrigger[Uml] => addOTIUMLTrigger(extent, uu)
    case uu: UMLUnmarshallAction[Uml] => addOTIUMLUnmarshallAction(extent, uu)
    case uu: UMLUsage[Uml] => addOTIUMLUsage(extent, uu)
    case uu: UMLUseCase[Uml] => addOTIUMLUseCase(extent, uu)
    case uu: UMLValuePin[Uml] => addOTIUMLValuePin(extent, uu)
    case uu: UMLValueSpecificationAction[Uml] => addOTIUMLValueSpecificationAction(extent, uu)
    case uu: UMLVariable[Uml] => addOTIUMLVariable(extent, uu)
    case uu: UMLAcceptEventAction[Uml] => addOTIUMLAcceptEventAction(extent, uu)
    case uu: UMLArtifact[Uml] => addOTIUMLArtifact(extent, uu)
    case uu: UMLAssociation[Uml] => addOTIUMLAssociation(extent, uu)
    case uu: UMLCentralBufferNode[Uml] => addOTIUMLCentralBufferNode(extent, uu)
    case uu: UMLCombinedFragment[Uml] => addOTIUMLCombinedFragment(extent, uu)
    case uu: UMLCreateLinkAction[Uml] => addOTIUMLCreateLinkAction(extent, uu)
    case uu: UMLDataType[Uml] => addOTIUMLDataType(extent, uu)
    case uu: UMLExpression[Uml] => addOTIUMLExpression(extent, uu)
    case uu: UMLInputPin[Uml] => addOTIUMLInputPin(extent, uu)
    case uu: UMLInstanceSpecification[Uml] => addOTIUMLInstanceSpecification(extent, uu)
    case uu: UMLInteractionUse[Uml] => addOTIUMLInteractionUse(extent, uu)
    case uu: UMLInterval[Uml] => addOTIUMLInterval(extent, uu)
    case uu: UMLIntervalConstraint[Uml] => addOTIUMLIntervalConstraint(extent, uu)
    case uu: UMLLinkEndData[Uml] => addOTIUMLLinkEndData(extent, uu)
    case uu: UMLMessageOccurrenceSpecification[Uml] => addOTIUMLMessageOccurrenceSpecification(extent, uu)
    case uu: UMLNode[Uml] => addOTIUMLNode(extent, uu)
    case uu: UMLOpaqueBehavior[Uml] => addOTIUMLOpaqueBehavior(extent, uu)
    case uu: UMLPackage[Uml] => addOTIUMLPackage(extent, uu)
    case uu: UMLProperty[Uml] => addOTIUMLProperty(extent, uu)
    case uu: UMLRealization[Uml] => addOTIUMLRealization(extent, uu)
    case uu: UMLState[Uml] => addOTIUMLState(extent, uu)
    case uu: UMLStateMachine[Uml] => addOTIUMLStateMachine(extent, uu)
    case uu: UMLStructuredActivityNode[Uml] => addOTIUMLStructuredActivityNode(extent, uu)
    case uu: UMLTemplateParameter[Uml] => addOTIUMLTemplateParameter(extent, uu)
    case uu: UMLTemplateSignature[Uml] => addOTIUMLTemplateSignature(extent, uu)
    case uu: UMLTransition[Uml] => addOTIUMLTransition(extent, uu)
    case uu: UMLAbstraction[Uml] => addOTIUMLAbstraction(extent, uu)
    case uu: UMLClass[Uml] => addOTIUMLClass(extent, uu)
    case uu: UMLConstraint[Uml] => addOTIUMLConstraint(extent, uu)
    case uu: UMLOccurrenceSpecification[Uml] => addOTIUMLOccurrenceSpecification(extent, uu)
    case uu: UMLDependency[Uml] => addOTIUMLDependency(extent, uu)
    case _ => extent
  }

  def addOTIUMLAbstraction
  (extent: OTIDocumentExtent,
   u: UMLAbstraction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction.apply _)
    val e2 =
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e6  
    result
  }

  def addOTIUMLAcceptCallAction
  (extent: OTIDocumentExtent,
   u: UMLAcceptCallAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.result, OTIUMLA_result_acceptEventAction.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.returnInformation, OTIUMLA_returnInformation_acceptCallAction.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.trigger, OTIUMLA_trigger_acceptEventAction.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e11  
    result
  }

  def addOTIUMLAcceptEventAction
  (extent: OTIDocumentExtent,
   u: UMLAcceptEventAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.result, OTIUMLA_result_acceptEventAction.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.trigger, OTIUMLA_trigger_acceptEventAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLActionExecutionSpecification
  (extent: OTIDocumentExtent,
   u: UMLActionExecutionSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.covered, OTIUMLA_covered_coveredBy.apply _)
    val result = e4  
    result
  }

  def addOTIUMLActionInputPin
  (extent: OTIDocumentExtent,
   u: UMLActionInputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.fromAction, OTIUMLA_fromAction_actionInputPin.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.inState, OTIUMLA_inState_objectNode.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLActivity
  (extent: OTIDocumentExtent,
   u: UMLActivity[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.edge, OTIUMLA_edge_activity.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.group, OTIUMLA_group_inActivity.apply _, u.structuredNode)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e8 = 
      toCompositeOrderedLinkExtent(e7, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.node, OTIUMLA_node_activity.apply _, u.structuredNode)
    val e10 = 
      toCompositeOrderedLinkExtent(e9, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e14 = 
      toCompositeOrderedLinkExtent(e13, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e15 = 
      toCompositeOrderedLinkExtent(e14, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior.apply _)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _, u.postcondition, u.precondition)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.postcondition, OTIUMLA_postcondition_behavior.apply _)
    val e23 = 
      toCompositeLinkExtent(e22, ud, u, u.precondition, OTIUMLA_precondition_behavior.apply _)
    val e24 = 
      toCompositeLinkExtent(e23, ud, u, u.structuredNode, OTIUMLA_structuredNode_activity.apply _)
    val e25 = 
      toCompositeLinkExtent(e24, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e26 = 
      toCompositeLinkExtent(e25, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e27 = 
      toCompositeLinkExtent(e26, ud, u, u.variable, OTIUMLA_variable_activityScope.apply _)
    val e28 =
      toReferenceLinkExtent(e27, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e29 =
      toReferenceLinkExtent(e28, ud, u, u.partition, OTIUMLA_partition_activity.apply _)
    val e30 =
      toReferenceLinkExtent(e29, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e31 =
      toReferenceLinkExtent(e30, ud, u, u.redefinedBehavior, OTIUMLA_redefinedBehavior_behavior.apply _)
    val e32 =
      toReferenceLinkExtent(e31, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _, u.redefinedBehavior)
    val e33 =
      toReferenceLinkExtent(e32, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e34 =
      toReferenceLinkExtent(e33, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e34  
    result
  }

  def addOTIUMLActivityFinalNode
  (extent: OTIDocumentExtent,
   u: UMLActivityFinalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e5  
    result
  }

  def addOTIUMLActivityParameterNode
  (extent: OTIDocumentExtent,
   u: UMLActivityParameterNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.inState, OTIUMLA_inState_objectNode.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e7  
    result
  }

  def addOTIUMLActivityPartition
  (extent: OTIDocumentExtent,
   u: UMLActivityPartition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.subpartition, OTIUMLA_subpartition_superPartition.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.edge, OTIUMLA_edge_inPartition.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.node, OTIUMLA_inPartition_node.apply _)
    val result = e5  
    result
  }

  def addOTIUMLActor
  (extent: OTIDocumentExtent,
   u: UMLActor[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e18  
    result
  }

  def addOTIUMLAddStructuralFeatureValueAction
  (extent: OTIDocumentExtent,
   u: UMLAddStructuralFeatureValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.insertAt, OTIUMLA_insertAt_addStructuralFeatureValueAction.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u._object, OTIUMLA_object_structuralFeatureAction.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.result, OTIUMLA_result_writeStructuralFeatureAction.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.value, OTIUMLA_value_writeStructuralFeatureAction.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e12 =
      toReferenceLinkExtent(e11, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e12  
    result
  }

  def addOTIUMLAddVariableValueAction
  (extent: OTIDocumentExtent,
   u: UMLAddVariableValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.insertAt, OTIUMLA_insertAt_addVariableValueAction.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.value, OTIUMLA_value_writeVariableAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLAnyReceiveEvent
  (extent: OTIDocumentExtent,
   u: UMLAnyReceiveEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLArtifact
  (extent: OTIDocumentExtent,
   u: UMLArtifact[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.manifestation, OTIUMLA_manifestation_artifact.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nestedArtifact, OTIUMLA_nestedArtifact_artifact.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_artifact.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e9 = 
      toCompositeOrderedLinkExtent(e8, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_artifact.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e19  
    result
  }

  def addOTIUMLAssociation
  (extent: OTIDocumentExtent,
   u: UMLAssociation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.ownedEnd, OTIUMLA_ownedEnd_owningAssociation.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e13 =
      toReferenceOrderedLinkExtent(e12, ud, u, u.memberEnd, OTIUMLA_memberEnd_association.apply _, u.navigableOwnedEnd)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.navigableOwnedEnd, OTIUMLA_navigableOwnedEnd_association.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e18  
    result
  }

  def addOTIUMLAssociationClass
  (extent: OTIDocumentExtent,
   u: UMLAssociationClass[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e11 = 
      toCompositeOrderedLinkExtent(e10, ud, u, u.ownedEnd, OTIUMLA_ownedEnd_owningAssociation.apply _)
    val e12 = 
      toCompositeOrderedLinkExtent(e11, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e21 =
      toReferenceOrderedLinkExtent(e20, ud, u, u.memberEnd, OTIUMLA_memberEnd_association.apply _, u.navigableOwnedEnd)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.navigableOwnedEnd, OTIUMLA_navigableOwnedEnd_association.apply _)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e26 =
      toReferenceLinkExtent(e25, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e26  
    result
  }

  def addOTIUMLBehaviorExecutionSpecification
  (extent: OTIDocumentExtent,
   u: UMLBehaviorExecutionSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.covered, OTIUMLA_covered_coveredBy.apply _)
    val result = e4  
    result
  }

  def addOTIUMLBroadcastSignalAction
  (extent: OTIDocumentExtent,
   u: UMLBroadcastSignalAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_invocationAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e9  
    result
  }

  def addOTIUMLCallBehaviorAction
  (extent: OTIDocumentExtent,
   u: UMLCallBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_invocationAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.result, OTIUMLA_result_callAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLCallEvent
  (extent: OTIDocumentExtent,
   u: UMLCallEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLCallOperationAction
  (extent: OTIDocumentExtent,
   u: UMLCallOperationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_invocationAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.result, OTIUMLA_result_callAction.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.target, OTIUMLA_target_callOperationAction.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e11  
    result
  }

  def addOTIUMLCentralBufferNode
  (extent: OTIDocumentExtent,
   u: UMLCentralBufferNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.inState, OTIUMLA_inState_objectNode.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e7  
    result
  }

  def addOTIUMLChangeEvent
  (extent: OTIDocumentExtent,
   u: UMLChangeEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.changeExpression, OTIUMLA_changeExpression_changeEvent.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e4  
    result
  }

  def addOTIUMLClass
  (extent: OTIDocumentExtent,
   u: UMLClass[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e11 = 
      toCompositeOrderedLinkExtent(e10, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e23  
    result
  }

  def addOTIUMLClassifierTemplateParameter
  (extent: OTIDocumentExtent,
   u: UMLClassifierTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.constrainingClassifier, OTIUMLA_constrainingClassifier_classifierTemplateParameter.apply _)
    val result = e4  
    result
  }

  def addOTIUMLClause
  (extent: OTIDocumentExtent,
   u: UMLClause[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 =
      toReferenceLinkExtent(e1, ud, u, u.body, OTIUMLA_body_clause.apply _)
    val e3 =
      toReferenceOrderedLinkExtent(e2, ud, u, u.bodyOutput, OTIUMLA_bodyOutput_clause.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.decider, OTIUMLA_decider_clause.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.predecessorClause, OTIUMLA_predecessorClause_successorClause.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.test, OTIUMLA_test_clause.apply _)
    val result = e6  
    result
  }

  def addOTIUMLClearAssociationAction
  (extent: OTIDocumentExtent,
   u: UMLClearAssociationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_clearAssociationAction.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.association, OTIUMLA_association_clearAssociationAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLClearStructuralFeatureAction
  (extent: OTIDocumentExtent,
   u: UMLClearStructuralFeatureAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_structuralFeatureAction.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_clearStructuralFeatureAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLClearVariableAction
  (extent: OTIDocumentExtent,
   u: UMLClearVariableAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e8  
    result
  }

  def addOTIUMLCollaboration
  (extent: OTIDocumentExtent,
   u: UMLCollaboration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_structuredClassifier.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.collaborationRole, OTIUMLA_collaborationRole_collaboration.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e21  
    result
  }

  def addOTIUMLCollaborationUse
  (extent: OTIDocumentExtent,
   u: UMLCollaborationUse[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.roleBinding, OTIUMLA_roleBinding_collaborationUse.apply _)
    val result = e3  
    result
  }

  def addOTIUMLCombinedFragment
  (extent: OTIDocumentExtent,
   u: UMLCombinedFragment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.cfragmentGate, OTIUMLA_cfragmentGate_combinedFragment.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e4 = 
      toCompositeOrderedLinkExtent(e3, ud, u, u.operand, OTIUMLA_operand_combinedFragment.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.covered, OTIUMLA_covered_coveredBy.apply _)
    val result = e6  
    result
  }

  def addOTIUMLComment
  (extent: OTIDocumentExtent,
   u: UMLComment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 =
      toReferenceLinkExtent(e1, ud, u, u.annotatedElement, OTIUMLA_annotatedElement_comment.apply _)
    val result = e2  
    result
  }

  def addOTIUMLCommunicationPath
  (extent: OTIDocumentExtent,
   u: UMLCommunicationPath[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.ownedEnd, OTIUMLA_ownedEnd_owningAssociation.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e13 =
      toReferenceOrderedLinkExtent(e12, ud, u, u.memberEnd, OTIUMLA_memberEnd_association.apply _, u.navigableOwnedEnd)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.navigableOwnedEnd, OTIUMLA_navigableOwnedEnd_association.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e18  
    result
  }

  def addOTIUMLComponent
  (extent: OTIDocumentExtent,
   u: UMLComponent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e11 = 
      toCompositeOrderedLinkExtent(e10, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.packagedElement, OTIUMLA_packagedElement_component.apply _)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.realization, OTIUMLA_realization_abstraction_component.apply _)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e25  
    result
  }

  def addOTIUMLComponentRealization
  (extent: OTIDocumentExtent,
   u: UMLComponentRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client.apply _, u.realizingClassifier)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.realizingClassifier, OTIUMLA_realizingClassifier_componentRealization.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e7  
    result
  }

  def addOTIUMLConditionalNode
  (extent: OTIDocumentExtent,
   u: UMLConditionalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.clause, OTIUMLA_clause_conditionalNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.edge, OTIUMLA_edge_inStructuredNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.node, OTIUMLA_node_inStructuredNode.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e12 = 
      toCompositeOrderedLinkExtent(e11, ud, u, u.result, OTIUMLA_result_conditionalNode.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.variable, OTIUMLA_variable_scope.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e17  
    result
  }

  def addOTIUMLConnectableElementTemplateParameter
  (extent: OTIDocumentExtent,
   u: UMLConnectableElementTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLConnectionPointReference
  (extent: OTIDocumentExtent,
   u: UMLConnectionPointReference[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.entry, OTIUMLA_entry_connectionPointReference.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.exit, OTIUMLA_exit_connectionPointReference.apply _)
    val result = e4  
    result
  }

  def addOTIUMLConnector
  (extent: OTIDocumentExtent,
   u: UMLConnector[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeOrderedLinkExtent(e0, ud, u, u.end, OTIUMLA_end_connector.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.contract, OTIUMLA_contract_connector.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedConnector, OTIUMLA_redefinedConnector_connector.apply _)
    val result = e5  
    result
  }

  def addOTIUMLConnectorEnd
  (extent: OTIDocumentExtent,
   u: UMLConnectorEnd[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper.apply _)
    val result = e3  
    result
  }

  def addOTIUMLConsiderIgnoreFragment
  (extent: OTIDocumentExtent,
   u: UMLConsiderIgnoreFragment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.cfragmentGate, OTIUMLA_cfragmentGate_combinedFragment.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e4 = 
      toCompositeOrderedLinkExtent(e3, ud, u, u.operand, OTIUMLA_operand_combinedFragment.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.covered, OTIUMLA_covered_coveredBy.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.message, OTIUMLA_message_considerIgnoreFragment.apply _)
    val result = e7  
    result
  }

  def addOTIUMLConstraint
  (extent: OTIDocumentExtent,
   u: UMLConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.specification, OTIUMLA_specification_owningConstraint.apply _)
    val e4 =
      toReferenceOrderedLinkExtent(e3, ud, u, u.constrainedElement, OTIUMLA_constrainedElement_constraint.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e5  
    result
  }

  def addOTIUMLContinuation
  (extent: OTIDocumentExtent,
   u: UMLContinuation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.covered, OTIUMLA_covered_coveredBy.apply _)
    val result = e4  
    result
  }

  def addOTIUMLControlFlow
  (extent: OTIDocumentExtent,
   u: UMLControlFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.guard, OTIUMLA_guard_activityEdge.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.weight, OTIUMLA_weight_activityEdge.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedEdge, OTIUMLA_redefinedEdge_activityEdge.apply _)
    val result = e5  
    result
  }

  def addOTIUMLCreateLinkAction
  (extent: OTIDocumentExtent,
   u: UMLCreateLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.endData, OTIUMLA_endData_createLinkAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.inputValue, OTIUMLA_inputValue_linkAction.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLCreateLinkObjectAction
  (extent: OTIDocumentExtent,
   u: UMLCreateLinkObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.endData, OTIUMLA_endData_createLinkAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.inputValue, OTIUMLA_inputValue_linkAction.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.result, OTIUMLA_result_createLinkObjectAction.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e11  
    result
  }

  def addOTIUMLCreateObjectAction
  (extent: OTIDocumentExtent,
   u: UMLCreateObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.result, OTIUMLA_result_createObjectAction.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e9  
    result
  }

  def addOTIUMLDataStoreNode
  (extent: OTIDocumentExtent,
   u: UMLDataStoreNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.inState, OTIUMLA_inState_objectNode.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e7  
    result
  }

  def addOTIUMLDataType
  (extent: OTIDocumentExtent,
   u: UMLDataType[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeOrderedLinkExtent(e4, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_datatype.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_datatype.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e17  
    result
  }

  def addOTIUMLDecisionNode
  (extent: OTIDocumentExtent,
   u: UMLDecisionNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.decisionInputFlow, OTIUMLA_decisionInputFlow_decisionNode.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e6  
    result
  }

  def addOTIUMLDependency
  (extent: OTIDocumentExtent,
   u: UMLDependency[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.client, OTIUMLA_clientDependency_client.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e5  
    result
  }

  def addOTIUMLDeployment
  (extent: OTIDocumentExtent,
   u: UMLDeployment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.configuration, OTIUMLA_configuration_deployment.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.deployedArtifact, OTIUMLA_deployedArtifact_deploymentForArtifact.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency.apply _, u.deployedArtifact)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e7  
    result
  }

  def addOTIUMLDeploymentSpecification
  (extent: OTIDocumentExtent,
   u: UMLDeploymentSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.manifestation, OTIUMLA_manifestation_artifact.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nestedArtifact, OTIUMLA_nestedArtifact_artifact.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_artifact.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e9 = 
      toCompositeOrderedLinkExtent(e8, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_artifact.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e19  
    result
  }

  def addOTIUMLDestroyLinkAction
  (extent: OTIDocumentExtent,
   u: UMLDestroyLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.endData, OTIUMLA_endData_destroyLinkAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.inputValue, OTIUMLA_inputValue_linkAction.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLDestroyObjectAction
  (extent: OTIDocumentExtent,
   u: UMLDestroyObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.target, OTIUMLA_target_destroyObjectAction.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e9  
    result
  }

  def addOTIUMLDestructionOccurrenceSpecification
  (extent: OTIDocumentExtent,
   u: UMLDestructionOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.message, OTIUMLA_message_messageEnd.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.toAfter, OTIUMLA_before_toAfter.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.toBefore, OTIUMLA_toBefore_after.apply _)
    val result = e6  
    result
  }

  def addOTIUMLDevice
  (extent: OTIDocumentExtent,
   u: UMLDevice[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nestedNode, OTIUMLA_nestedNode_node.apply _)
    val e9 = 
      toCompositeOrderedLinkExtent(e8, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e13 = 
      toCompositeOrderedLinkExtent(e12, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e25  
    result
  }

  def addOTIUMLDuration
  (extent: OTIDocumentExtent,
   u: UMLDuration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.expr, OTIUMLA_expr_duration.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.observation, OTIUMLA_observation_duration.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e5  
    result
  }

  def addOTIUMLDurationConstraint
  (extent: OTIDocumentExtent,
   u: UMLDurationConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.specification, OTIUMLA_specification_durationConstraint.apply _)
    val e4 =
      toReferenceOrderedLinkExtent(e3, ud, u, u.constrainedElement, OTIUMLA_constrainedElement_constraint.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e5  
    result
  }

  def addOTIUMLDurationInterval
  (extent: OTIDocumentExtent,
   u: UMLDurationInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLDurationObservation
  (extent: OTIDocumentExtent,
   u: UMLDurationObservation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceOrderedLinkExtent(e2, ud, u, u.event, OTIUMLA_event_durationObservation.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e4  
    result
  }

  def addOTIUMLElementImport
  (extent: OTIDocumentExtent,
   u: UMLElementImport[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e1  
    result
  }

  def addOTIUMLElementValue
  (extent: OTIDocumentExtent,
   u: UMLElementValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLEnumeration
  (extent: OTIDocumentExtent,
   u: UMLEnumeration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeOrderedLinkExtent(e4, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_datatype.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.ownedLiteral, OTIUMLA_ownedLiteral_enumeration.apply _)
    val e8 = 
      toCompositeOrderedLinkExtent(e7, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_datatype.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e18  
    result
  }

  def addOTIUMLEnumerationLiteral
  (extent: OTIDocumentExtent,
   u: UMLEnumerationLiteral[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.deployment, OTIUMLA_deployment_location.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.slot, OTIUMLA_slot_owningInstance.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.specification, OTIUMLA_specification_owningInstanceSpec.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e6  
    result
  }

  def addOTIUMLExceptionHandler
  (extent: OTIDocumentExtent,
   u: UMLExceptionHandler[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 =
      toReferenceLinkExtent(e1, ud, u, u.exceptionType, OTIUMLA_exceptionType_exceptionHandler.apply _)
    val result = e2  
    result
  }

  def addOTIUMLExecutionEnvironment
  (extent: OTIDocumentExtent,
   u: UMLExecutionEnvironment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nestedNode, OTIUMLA_nestedNode_node.apply _)
    val e9 = 
      toCompositeOrderedLinkExtent(e8, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e13 = 
      toCompositeOrderedLinkExtent(e12, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e25  
    result
  }

  def addOTIUMLExecutionOccurrenceSpecification
  (extent: OTIDocumentExtent,
   u: UMLExecutionOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.execution, OTIUMLA_execution_executionOccurrenceSpecification.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.toAfter, OTIUMLA_before_toAfter.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.toBefore, OTIUMLA_toBefore_after.apply _)
    val result = e6  
    result
  }

  def addOTIUMLExpansionNode
  (extent: OTIDocumentExtent,
   u: UMLExpansionNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.inState, OTIUMLA_inState_objectNode.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e7  
    result
  }

  def addOTIUMLExpansionRegion
  (extent: OTIDocumentExtent,
   u: UMLExpansionRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.edge, OTIUMLA_edge_inStructuredNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.node, OTIUMLA_node_inStructuredNode.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.structuredNodeOutput, OTIUMLA_structuredNodeOutput_structuredActivityNode.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.variable, OTIUMLA_variable_scope.apply _)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.inputElement, OTIUMLA_inputElement_regionAsInput.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.outputElement, OTIUMLA_outputElement_regionAsOutput.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e18  
    result
  }

  def addOTIUMLExpression
  (extent: OTIDocumentExtent,
   u: UMLExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeOrderedLinkExtent(e1, ud, u, u.operand, OTIUMLA_operand_expression.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e4  
    result
  }

  def addOTIUMLExtend
  (extent: OTIDocumentExtent,
   u: UMLExtend[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.condition, OTIUMLA_condition_extend.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceOrderedLinkExtent(e3, ud, u, u.extensionLocation, OTIUMLA_extensionLocation_extension.apply _)
    val result = e4  
    result
  }

  def addOTIUMLExtension
  (extent: OTIDocumentExtent,
   u: UMLExtension[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedEnd, OTIUMLA_ownedEnd_extension.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e13 =
      toReferenceOrderedLinkExtent(e12, ud, u, u.memberEnd, OTIUMLA_memberEnd_association.apply _, u.navigableOwnedEnd)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.navigableOwnedEnd, OTIUMLA_navigableOwnedEnd_association.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e18  
    result
  }

  def addOTIUMLExtensionEnd
  (extent: OTIDocumentExtent,
   u: UMLExtensionEnd[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.defaultValue, OTIUMLA_defaultValue_owningProperty.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.qualifier, OTIUMLA_qualifier_associationEnd.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.redefinedProperty, OTIUMLA_redefinedProperty_property.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.subsettedProperty, OTIUMLA_subsettedProperty_property.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement.apply _)
    val result = e10  
    result
  }

  def addOTIUMLExtensionPoint
  (extent: OTIDocumentExtent,
   u: UMLExtensionPoint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e2  
    result
  }

  def addOTIUMLFinalState
  (extent: OTIDocumentExtent,
   u: UMLFinalState[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.connection, OTIUMLA_connection_state.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.connectionPoint, OTIUMLA_connectionPoint_state.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.deferrableTrigger, OTIUMLA_deferrableTrigger_state.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.doActivity, OTIUMLA_doActivity_state.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.entry, OTIUMLA_entry_state.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.exit, OTIUMLA_exit_state.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _, u.stateInvariant)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.region, OTIUMLA_region_state.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.stateInvariant, OTIUMLA_stateInvariant_owningState.apply _)
    val result = e13  
    result
  }

  def addOTIUMLFlowFinalNode
  (extent: OTIDocumentExtent,
   u: UMLFlowFinalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e5  
    result
  }

  def addOTIUMLForkNode
  (extent: OTIDocumentExtent,
   u: UMLForkNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e5  
    result
  }

  def addOTIUMLFunctionBehavior
  (extent: OTIDocumentExtent,
   u: UMLFunctionBehavior[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e11 = 
      toCompositeOrderedLinkExtent(e10, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e12 = 
      toCompositeOrderedLinkExtent(e11, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _, u.postcondition, u.precondition)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.postcondition, OTIUMLA_postcondition_behavior.apply _)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.precondition, OTIUMLA_precondition_behavior.apply _)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.redefinedBehavior, OTIUMLA_redefinedBehavior_behavior.apply _)
    val e26 =
      toReferenceLinkExtent(e25, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _, u.redefinedBehavior)
    val e27 =
      toReferenceLinkExtent(e26, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e28 =
      toReferenceLinkExtent(e27, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e28  
    result
  }

  def addOTIUMLGate
  (extent: OTIDocumentExtent,
   u: UMLGate[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.message, OTIUMLA_message_messageEnd.apply _)
    val result = e3  
    result
  }

  def addOTIUMLGeneralOrdering
  (extent: OTIDocumentExtent,
   u: UMLGeneralOrdering[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e2  
    result
  }

  def addOTIUMLGeneralization
  (extent: OTIDocumentExtent,
   u: UMLGeneralization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e1  
    result
  }

  def addOTIUMLGeneralizationSet
  (extent: OTIDocumentExtent,
   u: UMLGeneralizationSet[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalizationSet_generalization.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e4  
    result
  }

  def addOTIUMLImage
  (extent: OTIDocumentExtent,
   u: UMLImage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e1  
    result
  }

  def addOTIUMLInclude
  (extent: OTIDocumentExtent,
   u: UMLInclude[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e2  
    result
  }

  def addOTIUMLInformationFlow
  (extent: OTIDocumentExtent,
   u: UMLInformationFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.conveyed, OTIUMLA_conveyed_conveyingFlow.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.informationSource, OTIUMLA_informationSource_informationFlow.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.informationTarget, OTIUMLA_informationTarget_informationFlow.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.realization, OTIUMLA_realization_abstraction_flow.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.realizingActivityEdge, OTIUMLA_realizingActivityEdge_informationFlow.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.realizingConnector, OTIUMLA_realizingConnector_informationFlow.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.realizingMessage, OTIUMLA_realizingMessage_informationFlow.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e10  
    result
  }

  def addOTIUMLInformationItem
  (extent: OTIDocumentExtent,
   u: UMLInformationItem[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e12 =
      toReferenceLinkExtent(e11, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e13 =
      toReferenceLinkExtent(e12, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.represented, OTIUMLA_represented_representation.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e16  
    result
  }

  def addOTIUMLInitialNode
  (extent: OTIDocumentExtent,
   u: UMLInitialNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e5  
    result
  }

  def addOTIUMLInputPin
  (extent: OTIDocumentExtent,
   u: UMLInputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.inState, OTIUMLA_inState_objectNode.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e9  
    result
  }

  def addOTIUMLInstanceSpecification
  (extent: OTIDocumentExtent,
   u: UMLInstanceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.deployment, OTIUMLA_deployment_location.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.slot, OTIUMLA_slot_owningInstance.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.specification, OTIUMLA_specification_owningInstanceSpec.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.classifier, OTIUMLA_classifier_instanceSpecification.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e7  
    result
  }

  def addOTIUMLInstanceValue
  (extent: OTIDocumentExtent,
   u: UMLInstanceValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLInteraction
  (extent: OTIDocumentExtent,
   u: UMLInteraction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.action, OTIUMLA_action_interaction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.formalGate, OTIUMLA_formalGate_interaction.apply _)
    val e5 = 
      toCompositeOrderedLinkExtent(e4, ud, u, u.fragment, OTIUMLA_fragment_enclosingInteraction.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.lifeline, OTIUMLA_lifeline_interaction.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.message, OTIUMLA_message_interaction.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e12 = 
      toCompositeOrderedLinkExtent(e11, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e13 = 
      toCompositeOrderedLinkExtent(e12, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e17 = 
      toCompositeOrderedLinkExtent(e16, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e18 = 
      toCompositeOrderedLinkExtent(e17, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior.apply _)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior.apply _)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _, u.postcondition, u.precondition)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e23 = 
      toCompositeLinkExtent(e22, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e24 = 
      toCompositeLinkExtent(e23, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e25 = 
      toCompositeLinkExtent(e24, ud, u, u.postcondition, OTIUMLA_postcondition_behavior.apply _)
    val e26 = 
      toCompositeLinkExtent(e25, ud, u, u.precondition, OTIUMLA_precondition_behavior.apply _)
    val e27 = 
      toCompositeLinkExtent(e26, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e28 = 
      toCompositeLinkExtent(e27, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e29 =
      toReferenceLinkExtent(e28, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e30 =
      toReferenceLinkExtent(e29, ud, u, u.covered, OTIUMLA_covered_coveredBy.apply _)
    val e31 =
      toReferenceLinkExtent(e30, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e32 =
      toReferenceLinkExtent(e31, ud, u, u.redefinedBehavior, OTIUMLA_redefinedBehavior_behavior.apply _)
    val e33 =
      toReferenceLinkExtent(e32, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _, u.redefinedBehavior)
    val e34 =
      toReferenceLinkExtent(e33, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e35 =
      toReferenceLinkExtent(e34, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e35  
    result
  }

  def addOTIUMLInteractionConstraint
  (extent: OTIDocumentExtent,
   u: UMLInteractionConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.maxint, OTIUMLA_maxint_interactionConstraint.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.minint, OTIUMLA_minint_interactionConstraint.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.specification, OTIUMLA_specification_owningConstraint.apply _)
    val e6 =
      toReferenceOrderedLinkExtent(e5, ud, u, u.constrainedElement, OTIUMLA_constrainedElement_constraint.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e7  
    result
  }

  def addOTIUMLInteractionOperand
  (extent: OTIDocumentExtent,
   u: UMLInteractionOperand[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e2 = 
      toCompositeOrderedLinkExtent(e1, ud, u, u.fragment, OTIUMLA_fragment_enclosingOperand.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.guard, OTIUMLA_guard_interactionOperand.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.covered, OTIUMLA_covered_coveredBy.apply _)
    val result = e9  
    result
  }

  def addOTIUMLInteractionUse
  (extent: OTIDocumentExtent,
   u: UMLInteractionUse[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.actualGate, OTIUMLA_actualGate_interactionUse.apply _)
    val e2 = 
      toCompositeOrderedLinkExtent(e1, ud, u, u.argument, OTIUMLA_argument_interactionUse.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.returnValue, OTIUMLA_returnValue_interactionUse.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.covered, OTIUMLA_covered_coveredBy.apply _)
    val result = e7  
    result
  }

  def addOTIUMLInterface
  (extent: OTIDocumentExtent,
   u: UMLInterface[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeOrderedLinkExtent(e4, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_interface.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_interface.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e8 = 
      toCompositeOrderedLinkExtent(e7, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_interface.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedReception, OTIUMLA_ownedReception_interface.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.protocol, OTIUMLA_protocol_interface.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _, u.redefinedInterface)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.redefinedInterface, OTIUMLA_redefinedInterface_interface.apply _)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e21  
    result
  }

  def addOTIUMLInterfaceRealization
  (extent: OTIDocumentExtent,
   u: UMLInterfaceRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e6  
    result
  }

  def addOTIUMLInterruptibleActivityRegion
  (extent: OTIDocumentExtent,
   u: UMLInterruptibleActivityRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.interruptingEdge, OTIUMLA_interruptingEdge_interrupts.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.node, OTIUMLA_inInterruptibleRegion_node.apply _)
    val result = e4  
    result
  }

  def addOTIUMLInterval
  (extent: OTIDocumentExtent,
   u: UMLInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLIntervalConstraint
  (extent: OTIDocumentExtent,
   u: UMLIntervalConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.specification, OTIUMLA_specification_intervalConstraint.apply _)
    val e4 =
      toReferenceOrderedLinkExtent(e3, ud, u, u.constrainedElement, OTIUMLA_constrainedElement_constraint.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e5  
    result
  }

  def addOTIUMLJoinNode
  (extent: OTIDocumentExtent,
   u: UMLJoinNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.joinSpec, OTIUMLA_joinSpec_joinNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e6  
    result
  }

  def addOTIUMLLifeline
  (extent: OTIDocumentExtent,
   u: UMLLifeline[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.selector, OTIUMLA_selector_lifeline.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.decomposedAs, OTIUMLA_decomposedAs_lifeline.apply _)
    val result = e4  
    result
  }

  def addOTIUMLLinkEndCreationData
  (extent: OTIDocumentExtent,
   u: UMLLinkEndCreationData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.qualifier, OTIUMLA_qualifier_linkEndData.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.insertAt, OTIUMLA_insertAt_linkEndCreationData.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.value, OTIUMLA_value_linkEndData.apply _)
    val result = e4  
    result
  }

  def addOTIUMLLinkEndData
  (extent: OTIDocumentExtent,
   u: UMLLinkEndData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.qualifier, OTIUMLA_qualifier_linkEndData.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.value, OTIUMLA_value_linkEndData.apply _)
    val result = e3  
    result
  }

  def addOTIUMLLinkEndDestructionData
  (extent: OTIDocumentExtent,
   u: UMLLinkEndDestructionData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.qualifier, OTIUMLA_qualifier_linkEndData.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.destroyAt, OTIUMLA_destroyAt_linkEndDestructionData.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.value, OTIUMLA_value_linkEndData.apply _)
    val result = e4  
    result
  }

  def addOTIUMLLiteralBoolean
  (extent: OTIDocumentExtent,
   u: UMLLiteralBoolean[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLLiteralInteger
  (extent: OTIDocumentExtent,
   u: UMLLiteralInteger[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLLiteralNull
  (extent: OTIDocumentExtent,
   u: UMLLiteralNull[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLLiteralReal
  (extent: OTIDocumentExtent,
   u: UMLLiteralReal[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLLiteralString
  (extent: OTIDocumentExtent,
   u: UMLLiteralString[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLLiteralUnlimitedNatural
  (extent: OTIDocumentExtent,
   u: UMLLiteralUnlimitedNatural[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLLoopNode
  (extent: OTIDocumentExtent,
   u: UMLLoopNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.edge, OTIUMLA_edge_inStructuredNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.loopVariable, OTIUMLA_loopVariable_loopNode.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.loopVariableInput, OTIUMLA_loopVariableInput_loopNode.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.node, OTIUMLA_node_inStructuredNode.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e13 = 
      toCompositeOrderedLinkExtent(e12, ud, u, u.result, OTIUMLA_result_loopNode.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.variable, OTIUMLA_variable_scope.apply _)
    val e15 =
      toReferenceOrderedLinkExtent(e14, ud, u, u.bodyOutput, OTIUMLA_bodyOutput_loopNode.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.bodyPart, OTIUMLA_bodyPart_loopNode.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.decider, OTIUMLA_decider_loopNode.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.setupPart, OTIUMLA_setupPart_loopNode.apply _)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.test, OTIUMLA_test_loopNode.apply _)
    val result = e22  
    result
  }

  def addOTIUMLManifestation
  (extent: OTIDocumentExtent,
   u: UMLManifestation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e6  
    result
  }

  def addOTIUMLMergeNode
  (extent: OTIDocumentExtent,
   u: UMLMergeNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e5  
    result
  }

  def addOTIUMLMessage
  (extent: OTIDocumentExtent,
   u: UMLMessage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_message.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.receiveEvent, OTIUMLA_receiveEvent_endMessage.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.sendEvent, OTIUMLA_sendEvent_endMessage.apply _)
    val result = e5  
    result
  }

  def addOTIUMLMessageOccurrenceSpecification
  (extent: OTIDocumentExtent,
   u: UMLMessageOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.message, OTIUMLA_message_messageEnd.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.toAfter, OTIUMLA_before_toAfter.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.toBefore, OTIUMLA_toBefore_after.apply _)
    val result = e6  
    result
  }

  def addOTIUMLModel
  (extent: OTIDocumentExtent,
   u: UMLModel[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.packageMerge, OTIUMLA_packageMerge_receivingPackage.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.packagedElement, OTIUMLA_packagedElement_owningPackage.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.profileApplication, OTIUMLA_profileApplication_applyingPackage.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e11  
    result
  }

  def addOTIUMLNode
  (extent: OTIDocumentExtent,
   u: UMLNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nestedNode, OTIUMLA_nestedNode_node.apply _)
    val e9 = 
      toCompositeOrderedLinkExtent(e8, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e13 = 
      toCompositeOrderedLinkExtent(e12, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e25  
    result
  }

  def addOTIUMLObjectFlow
  (extent: OTIDocumentExtent,
   u: UMLObjectFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.guard, OTIUMLA_guard_activityEdge.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.weight, OTIUMLA_weight_activityEdge.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.redefinedEdge, OTIUMLA_redefinedEdge_activityEdge.apply _)
    val result = e5  
    result
  }

  def addOTIUMLOccurrenceSpecification
  (extent: OTIDocumentExtent,
   u: UMLOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.toAfter, OTIUMLA_before_toAfter.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.toBefore, OTIUMLA_toBefore_after.apply _)
    val result = e5  
    result
  }

  def addOTIUMLOpaqueAction
  (extent: OTIDocumentExtent,
   u: UMLOpaqueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.inputValue, OTIUMLA_inputValue_opaqueAction.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.outputValue, OTIUMLA_outputValue_opaqueAction.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLOpaqueBehavior
  (extent: OTIDocumentExtent,
   u: UMLOpaqueBehavior[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e11 = 
      toCompositeOrderedLinkExtent(e10, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e12 = 
      toCompositeOrderedLinkExtent(e11, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _, u.postcondition, u.precondition)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.postcondition, OTIUMLA_postcondition_behavior.apply _)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.precondition, OTIUMLA_precondition_behavior.apply _)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.redefinedBehavior, OTIUMLA_redefinedBehavior_behavior.apply _)
    val e26 =
      toReferenceLinkExtent(e25, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _, u.redefinedBehavior)
    val e27 =
      toReferenceLinkExtent(e26, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e28 =
      toReferenceLinkExtent(e27, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e28  
    result
  }

  def addOTIUMLOpaqueExpression
  (extent: OTIDocumentExtent,
   u: UMLOpaqueExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLOperation
  (extent: OTIDocumentExtent,
   u: UMLOperation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.bodyCondition, OTIUMLA_bodyCondition_bodyContext.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e5 = 
      toCompositeOrderedLinkExtent(e4, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_operation.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavioralFeature.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _, u.bodyCondition, u.postcondition, u.precondition)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.postcondition, OTIUMLA_postcondition_postContext.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.precondition, OTIUMLA_precondition_preContext.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e13 =
      toReferenceLinkExtent(e12, ud, u, u.method, OTIUMLA_method_specification.apply _)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.raisedException, OTIUMLA_raisedException_operation.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.redefinedOperation, OTIUMLA_redefinedOperation_operation.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.templateParameter, OTIUMLA_operation_templateParameter_parameteredElement.apply _)
    val result = e16  
    result
  }

  def addOTIUMLOperationTemplateParameter
  (extent: OTIDocumentExtent,
   u: UMLOperationTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLOutputPin
  (extent: OTIDocumentExtent,
   u: UMLOutputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.inState, OTIUMLA_inState_objectNode.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e9  
    result
  }

  def addOTIUMLPackage
  (extent: OTIDocumentExtent,
   u: UMLPackage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.packageMerge, OTIUMLA_packageMerge_receivingPackage.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.packagedElement, OTIUMLA_packagedElement_owningPackage.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.profileApplication, OTIUMLA_profileApplication_applyingPackage.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e11  
    result
  }

  def addOTIUMLPackageImport
  (extent: OTIDocumentExtent,
   u: UMLPackageImport[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e1  
    result
  }

  def addOTIUMLPackageMerge
  (extent: OTIDocumentExtent,
   u: UMLPackageMerge[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e1  
    result
  }

  def addOTIUMLParameter
  (extent: OTIDocumentExtent,
   u: UMLParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.defaultValue, OTIUMLA_defaultValue_owningParameter.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.parameterSet, OTIUMLA_parameterSet_parameter.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement.apply _)
    val result = e7  
    result
  }

  def addOTIUMLParameterSet
  (extent: OTIDocumentExtent,
   u: UMLParameterSet[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.condition, OTIUMLA_condition_parameterSet.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e3  
    result
  }

  def addOTIUMLPartDecomposition
  (extent: OTIDocumentExtent,
   u: UMLPartDecomposition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.actualGate, OTIUMLA_actualGate_interactionUse.apply _)
    val e2 = 
      toCompositeOrderedLinkExtent(e1, ud, u, u.argument, OTIUMLA_argument_interactionUse.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.returnValue, OTIUMLA_returnValue_interactionUse.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.covered, OTIUMLA_covered_coveredBy.apply _)
    val result = e7  
    result
  }

  def addOTIUMLPort
  (extent: OTIDocumentExtent,
   u: UMLPort[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.defaultValue, OTIUMLA_defaultValue_owningProperty.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.qualifier, OTIUMLA_qualifier_associationEnd.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.redefinedPort, OTIUMLA_redefinedPort_port.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedProperty, OTIUMLA_redefinedProperty_property.apply _, u.redefinedPort)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.subsettedProperty, OTIUMLA_subsettedProperty_property.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement.apply _)
    val result = e11  
    result
  }

  def addOTIUMLPrimitiveType
  (extent: OTIDocumentExtent,
   u: UMLPrimitiveType[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeOrderedLinkExtent(e4, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_datatype.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_datatype.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e17  
    result
  }

  def addOTIUMLProfile
  (extent: OTIDocumentExtent,
   u: UMLProfile[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _, u.metaclassReference)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.metaclassReference, OTIUMLA_metaclassReference_profile.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.metamodelReference, OTIUMLA_metamodelReference_profile.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _, u.metamodelReference)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.packageMerge, OTIUMLA_packageMerge_receivingPackage.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packagedElement, OTIUMLA_packagedElement_owningPackage.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.profileApplication, OTIUMLA_profileApplication_applyingPackage.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e13 =
      toReferenceLinkExtent(e12, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e13  
    result
  }

  def addOTIUMLProfileApplication
  (extent: OTIDocumentExtent,
   u: UMLProfileApplication[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e1  
    result
  }

  def addOTIUMLProperty
  (extent: OTIDocumentExtent,
   u: UMLProperty[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.defaultValue, OTIUMLA_defaultValue_owningProperty.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.deployment, OTIUMLA_deployment_location.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.qualifier, OTIUMLA_qualifier_associationEnd.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.redefinedProperty, OTIUMLA_redefinedProperty_property.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.subsettedProperty, OTIUMLA_subsettedProperty_property.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement.apply _)
    val result = e10  
    result
  }

  def addOTIUMLProtocolConformance
  (extent: OTIDocumentExtent,
   u: UMLProtocolConformance[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e1  
    result
  }

  def addOTIUMLProtocolStateMachine
  (extent: OTIDocumentExtent,
   u: UMLProtocolStateMachine[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.conformance, OTIUMLA_conformance_specificMachine.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.connectionPoint, OTIUMLA_connectionPoint_stateMachine.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e8 = 
      toCompositeOrderedLinkExtent(e7, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e9 = 
      toCompositeOrderedLinkExtent(e8, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e13 = 
      toCompositeOrderedLinkExtent(e12, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e14 = 
      toCompositeOrderedLinkExtent(e13, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _, u.postcondition, u.precondition)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.postcondition, OTIUMLA_postcondition_behavior.apply _)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.precondition, OTIUMLA_precondition_behavior.apply _)
    val e23 = 
      toCompositeLinkExtent(e22, ud, u, u.region, OTIUMLA_region_stateMachine.apply _)
    val e24 = 
      toCompositeLinkExtent(e23, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e25 = 
      toCompositeLinkExtent(e24, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e26 =
      toReferenceLinkExtent(e25, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e27 =
      toReferenceLinkExtent(e26, ud, u, u.extendedStateMachine, OTIUMLA_extendedStateMachine_stateMachine.apply _)
    val e28 =
      toReferenceLinkExtent(e27, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e29 =
      toReferenceLinkExtent(e28, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _, u.extendedStateMachine)
    val e30 =
      toReferenceLinkExtent(e29, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e31 =
      toReferenceLinkExtent(e30, ud, u, u.submachineState, OTIUMLA_submachineState_submachine.apply _)
    val e32 =
      toReferenceLinkExtent(e31, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e32  
    result
  }

  def addOTIUMLProtocolTransition
  (extent: OTIDocumentExtent,
   u: UMLProtocolTransition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.effect, OTIUMLA_effect_transition.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.guard, OTIUMLA_guard_transition.apply _, u.preCondition)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _, u.guard, u.postCondition, u.preCondition)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.postCondition, OTIUMLA_postCondition_owningTransition.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.preCondition, OTIUMLA_preCondition_protocolTransition.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.trigger, OTIUMLA_trigger_transition.apply _)
    val result = e10  
    result
  }

  def addOTIUMLPseudostate
  (extent: OTIDocumentExtent,
   u: UMLPseudostate[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e2  
    result
  }

  def addOTIUMLQualifierValue
  (extent: OTIDocumentExtent,
   u: UMLQualifierValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 =
      toReferenceLinkExtent(e1, ud, u, u.value, OTIUMLA_value_qualifierValue.apply _)
    val result = e2  
    result
  }

  def addOTIUMLRaiseExceptionAction
  (extent: OTIDocumentExtent,
   u: UMLRaiseExceptionAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.exception, OTIUMLA_exception_raiseExceptionAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e9  
    result
  }

  def addOTIUMLReadExtentAction
  (extent: OTIDocumentExtent,
   u: UMLReadExtentAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.result, OTIUMLA_result_readExtentAction.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.classifier, OTIUMLA_classifier_readExtentAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLReadIsClassifiedObjectAction
  (extent: OTIDocumentExtent,
   u: UMLReadIsClassifiedObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_readIsClassifiedObjectAction.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_readIsClassifiedObjectAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLReadLinkAction
  (extent: OTIDocumentExtent,
   u: UMLReadLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.endData, OTIUMLA_endData_linkAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.inputValue, OTIUMLA_inputValue_linkAction.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.result, OTIUMLA_result_readLinkAction.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e11  
    result
  }

  def addOTIUMLReadLinkObjectEndAction
  (extent: OTIDocumentExtent,
   u: UMLReadLinkObjectEndAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_readLinkObjectEndAction.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_readLinkObjectEndAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.end, OTIUMLA_end_readLinkObjectEndAction.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e11  
    result
  }

  def addOTIUMLReadLinkObjectEndQualifierAction
  (extent: OTIDocumentExtent,
   u: UMLReadLinkObjectEndQualifierAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_readLinkObjectEndQualifierAction.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_readLinkObjectEndQualifierAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.qualifier, OTIUMLA_qualifier_readLinkObjectEndQualifierAction.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e11  
    result
  }

  def addOTIUMLReadSelfAction
  (extent: OTIDocumentExtent,
   u: UMLReadSelfAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.result, OTIUMLA_result_readSelfAction.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e9  
    result
  }

  def addOTIUMLReadStructuralFeatureAction
  (extent: OTIDocumentExtent,
   u: UMLReadStructuralFeatureAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_structuralFeatureAction.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_readStructuralFeatureAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLReadVariableAction
  (extent: OTIDocumentExtent,
   u: UMLReadVariableAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.result, OTIUMLA_result_readVariableAction.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e9  
    result
  }

  def addOTIUMLRealization
  (extent: OTIDocumentExtent,
   u: UMLRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e6  
    result
  }

  def addOTIUMLReception
  (extent: OTIDocumentExtent,
   u: UMLReception[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeOrderedLinkExtent(e3, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_ownerFormalParam.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavioralFeature.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.method, OTIUMLA_method_specification.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.raisedException, OTIUMLA_raisedException_behavioralFeature.apply _)
    val result = e9  
    result
  }

  def addOTIUMLReclassifyObjectAction
  (extent: OTIDocumentExtent,
   u: UMLReclassifyObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_reclassifyObjectAction.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.newClassifier, OTIUMLA_newClassifier_reclassifyObjectAction.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.oldClassifier, OTIUMLA_oldClassifier_reclassifyObjectAction.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e11  
    result
  }

  def addOTIUMLRedefinableTemplateSignature
  (extent: OTIDocumentExtent,
   u: UMLRedefinableTemplateSignature[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeOrderedLinkExtent(e2, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_signature.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.extendedSignature, OTIUMLA_extendedSignature_redefinableTemplateSignature.apply _)
    val e5 =
      toReferenceOrderedLinkExtent(e4, ud, u, u.parameter, OTIUMLA_parameter_templateSignature.apply _)
    val result = e5  
    result
  }

  def addOTIUMLReduceAction
  (extent: OTIDocumentExtent,
   u: UMLReduceAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collection, OTIUMLA_collection_reduceAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_reduceAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLRegion
  (extent: OTIDocumentExtent,
   u: UMLRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.subvertex, OTIUMLA_subvertex_container.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.transition, OTIUMLA_transition_container.apply _)
    val result = e7  
    result
  }

  def addOTIUMLRemoveStructuralFeatureValueAction
  (extent: OTIDocumentExtent,
   u: UMLRemoveStructuralFeatureValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_structuralFeatureAction.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.removeAt, OTIUMLA_removeAt_removeStructuralFeatureValueAction.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.result, OTIUMLA_result_writeStructuralFeatureAction.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.value, OTIUMLA_value_writeStructuralFeatureAction.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e12 =
      toReferenceLinkExtent(e11, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e12  
    result
  }

  def addOTIUMLRemoveVariableValueAction
  (extent: OTIDocumentExtent,
   u: UMLRemoveVariableValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.removeAt, OTIUMLA_removeAt_removeVariableValueAction.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.value, OTIUMLA_value_writeVariableAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLReplyAction
  (extent: OTIDocumentExtent,
   u: UMLReplyAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeOrderedLinkExtent(e5, ud, u, u.replyValue, OTIUMLA_replyValue_replyAction.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.returnInformation, OTIUMLA_returnInformation_replyAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.replyToCall, OTIUMLA_replyToCall_replyAction.apply _)
    val result = e11  
    result
  }

  def addOTIUMLSendObjectAction
  (extent: OTIDocumentExtent,
   u: UMLSendObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.request, OTIUMLA_request_sendObjectAction.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.target, OTIUMLA_target_sendObjectAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLSendSignalAction
  (extent: OTIDocumentExtent,
   u: UMLSendSignalAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_invocationAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.target, OTIUMLA_target_sendSignalAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLSequenceNode
  (extent: OTIDocumentExtent,
   u: UMLSequenceNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.edge, OTIUMLA_edge_inStructuredNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeOrderedLinkExtent(e2, ud, u, u.executableNode, OTIUMLA_executableNode_sequenceNode.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.structuredNodeOutput, OTIUMLA_structuredNodeOutput_structuredActivityNode.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.variable, OTIUMLA_variable_scope.apply _)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e16  
    result
  }

  def addOTIUMLSignal
  (extent: OTIDocumentExtent,
   u: UMLSignal[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeOrderedLinkExtent(e4, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_owningSignal.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e13 =
      toReferenceLinkExtent(e12, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e16  
    result
  }

  def addOTIUMLSignalEvent
  (extent: OTIDocumentExtent,
   u: UMLSignalEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLSlot
  (extent: OTIDocumentExtent,
   u: UMLSlot[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 = 
      toCompositeOrderedLinkExtent(e1, ud, u, u.value, OTIUMLA_value_owningSlot.apply _)
    val result = e2  
    result
  }

  def addOTIUMLStartClassifierBehaviorAction
  (extent: OTIDocumentExtent,
   u: UMLStartClassifierBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_startClassifierBehaviorAction.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e9  
    result
  }

  def addOTIUMLStartObjectBehaviorAction
  (extent: OTIDocumentExtent,
   u: UMLStartObjectBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeOrderedLinkExtent(e0, ud, u, u.argument, OTIUMLA_argument_invocationAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u._object, OTIUMLA_object_startObjectBehaviorAction.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e8 = 
      toCompositeOrderedLinkExtent(e7, ud, u, u.result, OTIUMLA_result_callAction.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e11  
    result
  }

  def addOTIUMLState
  (extent: OTIDocumentExtent,
   u: UMLState[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.connection, OTIUMLA_connection_state.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.connectionPoint, OTIUMLA_connectionPoint_state.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.deferrableTrigger, OTIUMLA_deferrableTrigger_state.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.doActivity, OTIUMLA_doActivity_state.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.entry, OTIUMLA_entry_state.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.exit, OTIUMLA_exit_state.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _, u.stateInvariant)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.region, OTIUMLA_region_state.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.stateInvariant, OTIUMLA_stateInvariant_owningState.apply _)
    val result = e13  
    result
  }

  def addOTIUMLStateInvariant
  (extent: OTIDocumentExtent,
   u: UMLStateInvariant[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.invariant, OTIUMLA_invariant_stateInvariant.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e4  
    result
  }

  def addOTIUMLStateMachine
  (extent: OTIDocumentExtent,
   u: UMLStateMachine[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.connectionPoint, OTIUMLA_connectionPoint_stateMachine.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e8 = 
      toCompositeOrderedLinkExtent(e7, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e12 = 
      toCompositeOrderedLinkExtent(e11, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e13 = 
      toCompositeOrderedLinkExtent(e12, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _, u.postcondition, u.precondition)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e20 = 
      toCompositeLinkExtent(e19, ud, u, u.postcondition, OTIUMLA_postcondition_behavior.apply _)
    val e21 = 
      toCompositeLinkExtent(e20, ud, u, u.precondition, OTIUMLA_precondition_behavior.apply _)
    val e22 = 
      toCompositeLinkExtent(e21, ud, u, u.region, OTIUMLA_region_stateMachine.apply _)
    val e23 = 
      toCompositeLinkExtent(e22, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e24 = 
      toCompositeLinkExtent(e23, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e25 =
      toReferenceLinkExtent(e24, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e26 =
      toReferenceLinkExtent(e25, ud, u, u.extendedStateMachine, OTIUMLA_extendedStateMachine_stateMachine.apply _)
    val e27 =
      toReferenceLinkExtent(e26, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e28 =
      toReferenceLinkExtent(e27, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _, u.extendedStateMachine)
    val e29 =
      toReferenceLinkExtent(e28, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e30 =
      toReferenceLinkExtent(e29, ud, u, u.submachineState, OTIUMLA_submachineState_submachine.apply _)
    val e31 =
      toReferenceLinkExtent(e30, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e31  
    result
  }

  def addOTIUMLStereotype
  (extent: OTIDocumentExtent,
   u: UMLStereotype[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.icon, OTIUMLA_icon_stereotype.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass.apply _)
    val e8 = 
      toCompositeOrderedLinkExtent(e7, ud, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier.apply _)
    val e12 = 
      toCompositeOrderedLinkExtent(e11, ud, u, u.ownedOperation, OTIUMLA_ownedOperation_class.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedReception, OTIUMLA_ownedReception_class.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e17 = 
      toCompositeLinkExtent(e16, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e18 = 
      toCompositeLinkExtent(e17, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e19 = 
      toCompositeLinkExtent(e18, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e23 =
      toReferenceLinkExtent(e22, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e24 =
      toReferenceLinkExtent(e23, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e24  
    result
  }

  def addOTIUMLStringExpression
  (extent: OTIDocumentExtent,
   u: UMLStringExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeOrderedLinkExtent(e1, ud, u, u.operand, OTIUMLA_operand_expression.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template.apply _)
    val e5 = 
      toCompositeOrderedLinkExtent(e4, ud, u, u.subExpression, OTIUMLA_subExpression_owningExpression.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e7  
    result
  }

  def addOTIUMLStructuredActivityNode
  (extent: OTIDocumentExtent,
   u: UMLStructuredActivityNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.edge, OTIUMLA_edge_inStructuredNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.node, OTIUMLA_node_inStructuredNode.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.structuredNodeOutput, OTIUMLA_structuredNodeOutput_structuredActivityNode.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.variable, OTIUMLA_variable_scope.apply _)
    val e14 =
      toReferenceLinkExtent(e13, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e15 =
      toReferenceLinkExtent(e14, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e16 =
      toReferenceLinkExtent(e15, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e16  
    result
  }

  def addOTIUMLSubstitution
  (extent: OTIDocumentExtent,
   u: UMLSubstitution[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.mapping, OTIUMLA_mapping_abstraction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.client, OTIUMLA_clientDependency_client.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency.apply _)
    val e6 =
      toReferenceLinkExtent(e5, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e6  
    result
  }

  def addOTIUMLTemplateBinding
  (extent: OTIDocumentExtent,
   u: UMLTemplateBinding[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.parameterSubstitution, OTIUMLA_parameterSubstitution_templateBinding.apply _)
    val result = e2  
    result
  }

  def addOTIUMLTemplateParameter
  (extent: OTIDocumentExtent,
   u: UMLTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLTemplateParameterSubstitution
  (extent: OTIDocumentExtent,
   u: UMLTemplateParameterSubstitution[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedActual, OTIUMLA_ownedActual_owningTemplateParameterSubstitution.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val result = e2  
    result
  }

  def addOTIUMLTemplateSignature
  (extent: OTIDocumentExtent,
   u: UMLTemplateSignature[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e2 = 
      toCompositeOrderedLinkExtent(e1, ud, u, u.ownedParameter, OTIUMLA_ownedParameter_signature.apply _)
    val e3 =
      toReferenceOrderedLinkExtent(e2, ud, u, u.parameter, OTIUMLA_parameter_templateSignature.apply _)
    val result = e3  
    result
  }

  def addOTIUMLTestIdentityAction
  (extent: OTIDocumentExtent,
   u: UMLTestIdentityAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.first, OTIUMLA_first_testIdentityAction.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.result, OTIUMLA_result_testIdentityAction.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.second, OTIUMLA_second_testIdentityAction.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e11 =
      toReferenceLinkExtent(e10, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e11  
    result
  }

  def addOTIUMLTimeConstraint
  (extent: OTIDocumentExtent,
   u: UMLTimeConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.specification, OTIUMLA_specification_timeConstraint.apply _)
    val e4 =
      toReferenceOrderedLinkExtent(e3, ud, u, u.constrainedElement, OTIUMLA_constrainedElement_constraint.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e5  
    result
  }

  def addOTIUMLTimeEvent
  (extent: OTIDocumentExtent,
   u: UMLTimeEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.when, OTIUMLA_when_timeEvent.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e4  
    result
  }

  def addOTIUMLTimeExpression
  (extent: OTIDocumentExtent,
   u: UMLTimeExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.expr, OTIUMLA_expr_timeExpression.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.observation, OTIUMLA_observation_timeExpression.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e5  
    result
  }

  def addOTIUMLTimeInterval
  (extent: OTIDocumentExtent,
   u: UMLTimeInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLTimeObservation
  (extent: OTIDocumentExtent,
   u: UMLTimeObservation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e3  
    result
  }

  def addOTIUMLTransition
  (extent: OTIDocumentExtent,
   u: UMLTransition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.effect, OTIUMLA_effect_transition.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.guard, OTIUMLA_guard_transition.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _, u.guard)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.trigger, OTIUMLA_trigger_transition.apply _)
    val result = e8  
    result
  }

  def addOTIUMLTrigger
  (extent: OTIDocumentExtent,
   u: UMLTrigger[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.port, OTIUMLA_port_trigger.apply _)
    val result = e3  
    result
  }

  def addOTIUMLUnmarshallAction
  (extent: OTIDocumentExtent,
   u: UMLUnmarshallAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u._object, OTIUMLA_object_unmarshallAction.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e7 = 
      toCompositeOrderedLinkExtent(e6, ud, u, u.result, OTIUMLA_result_unmarshallAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLUsage
  (extent: OTIDocumentExtent,
   u: UMLUsage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e3 =
      toReferenceLinkExtent(e2, ud, u, u.client, OTIUMLA_clientDependency_client.apply _)
    val e4 =
      toReferenceLinkExtent(e3, ud, u, u.supplier, OTIUMLA_supplier_supplierDependency.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_parameteredElement_templateParameter.apply _)
    val result = e5  
    result
  }

  def addOTIUMLUseCase
  (extent: OTIDocumentExtent,
   u: UMLUseCase[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.elementImport, OTIUMLA_elementImport_importingNamespace.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.extend, OTIUMLA_extend_extension.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.extensionPoint, OTIUMLA_extensionPoint_useCase.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.generalization, OTIUMLA_generalization_specific.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.include, OTIUMLA_include_includingCase.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier.apply _)
    val e8 = 
      toCompositeLinkExtent(e7, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e9 = 
      toCompositeLinkExtent(e8, ud, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier.apply _)
    val e10 = 
      toCompositeLinkExtent(e9, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e11 = 
      toCompositeLinkExtent(e10, ud, u, u.ownedRule, OTIUMLA_ownedRule_context.apply _)
    val e12 = 
      toCompositeLinkExtent(e11, ud, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier.apply _)
    val e13 = 
      toCompositeLinkExtent(e12, ud, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier.apply _)
    val e14 = 
      toCompositeLinkExtent(e13, ud, u, u.packageImport, OTIUMLA_packageImport_importingNamespace.apply _)
    val e15 = 
      toCompositeLinkExtent(e14, ud, u, u.substitution, OTIUMLA_substitution_substitutingClassifier.apply _)
    val e16 = 
      toCompositeLinkExtent(e15, ud, u, u.templateBinding, OTIUMLA_templateBinding_boundElement.apply _)
    val e17 =
      toReferenceLinkExtent(e16, ud, u, u.classifierBehavior, OTIUMLA_classifierBehavior_behavioredClassifier.apply _)
    val e18 =
      toReferenceLinkExtent(e17, ud, u, u.powertypeExtent, OTIUMLA_powertypeExtent_powertype.apply _)
    val e19 =
      toReferenceLinkExtent(e18, ud, u, u.redefinedClassifier, OTIUMLA_redefinedClassifier_classifier.apply _)
    val e20 =
      toReferenceLinkExtent(e19, ud, u, u.representation, OTIUMLA_representation_classifier.apply _)
    val e21 =
      toReferenceLinkExtent(e20, ud, u, u.subject, OTIUMLA_subject_useCase.apply _)
    val e22 =
      toReferenceLinkExtent(e21, ud, u, u.templateParameter, OTIUMLA_classifier_templateParameter_parameteredElement.apply _)
    val result = e22  
    result
  }

  def addOTIUMLValuePin
  (extent: OTIDocumentExtent,
   u: UMLValuePin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.upperBound, OTIUMLA_upperBound_objectNode.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.value, OTIUMLA_value_valuePin.apply _)
    val e7 =
      toReferenceLinkExtent(e6, ud, u, u.inState, OTIUMLA_inState_objectNode.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLValueSpecificationAction
  (extent: OTIDocumentExtent,
   u: UMLValueSpecificationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.handler, OTIUMLA_handler_protectedNode.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.localPostcondition, OTIUMLA_localPostcondition_action.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.localPrecondition, OTIUMLA_localPrecondition_action.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e5 = 
      toCompositeLinkExtent(e4, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e6 = 
      toCompositeLinkExtent(e5, ud, u, u.result, OTIUMLA_result_valueSpecificationAction.apply _)
    val e7 = 
      toCompositeLinkExtent(e6, ud, u, u.value, OTIUMLA_value_valueSpecificationAction.apply _)
    val e8 =
      toReferenceLinkExtent(e7, ud, u, u.incoming, OTIUMLA_incoming_target_node.apply _)
    val e9 =
      toReferenceLinkExtent(e8, ud, u, u.outgoing, OTIUMLA_outgoing_source_node.apply _)
    val e10 =
      toReferenceLinkExtent(e9, ud, u, u.redefinedNode, OTIUMLA_redefinedNode_activityNode.apply _)
    val result = e10  
    result
  }

  def addOTIUMLVariable
  (extent: OTIDocumentExtent,
   u: UMLVariable[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = odsa.ds.lookupDocumentByExtent(u).fold[OTIDocumentExtent](extent){ ud =>
    val e0 = extent.copy(
      elementExtent = 
        extent.elementExtent :+
          otiJsonElementHelper.toOTIMOFElement(u, Some(ud)))
    val e1 = 
      toCompositeLinkExtent(e0, ud, u, u.lowerValue, OTIUMLA_lowerValue_owningLower.apply _)
    val e2 = 
      toCompositeLinkExtent(e1, ud, u, u.nameExpression, OTIUMLA_nameExpression_namedElement.apply _)
    val e3 = 
      toCompositeLinkExtent(e2, ud, u, u.ownedComment, OTIUMLA_ownedComment_owningElement.apply _)
    val e4 = 
      toCompositeLinkExtent(e3, ud, u, u.upperValue, OTIUMLA_upperValue_owningUpper.apply _)
    val e5 =
      toReferenceLinkExtent(e4, ud, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement.apply _)
    val result = e5  
    result
  }
}
