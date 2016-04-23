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
import org.omg.oti.uml.canonicalXMI.DocumentSet
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.canonicalXMI.helper.OTIAdapter
import org.omg.oti.uml.write.api.{UMLFactory, UMLUpdate}
import org.omg.oti.uml.xmi.Document
import org.omg.oti.json.common._
import org.omg.oti.json.common.OTIPrimitiveTypes.TOOL_SPECIFIC_ID
import org.omg.oti.json.uml._
import org.omg.oti.json.uml.enums._

import scala.collection.immutable._
import scala.{Boolean,Double,Int,Option,None}
import scala.Predef.{require,String}
import scalaz.@@
// <!-- End of user code imports -->

object OTIJsonElementHelper {

  // <!-- Start of user code companion -->
  // <!-- End of user code companion -->

}

case class OTIJsonElementHelper
[Uml <: UML,
 Uo <: UMLOps[Uml],
 Ch <: OTICharacteristicsProvider[Uml],
 Uf <: UMLFactory[Uml],
 Uu <: UMLUpdate[Uml],
 Ds <: DocumentSet[Uml]]
( oa: OTIAdapter[Uml, Uo, Ch, Uf, Uu],
  ds: Option[Ds]) {

  // <!-- Start of user code additions -->

  implicit def optionToOTI[U,V]
  (value: Option[U])
  (implicit u2v: U => V)
  : Option[V]
  = value.map(u2v)

  implicit def toOTIMOFElement
  (u: Uml#Element)
  : OTIMOFElement
  = toOTIMOFElement(oa.umlOps.cacheLookupOrUpdate(u), Option.empty[Document[Uml]])

  implicit def toOTIMOFElement
  (u: UMLElement[Uml])
  : OTIMOFElement
  = toOTIMOFElement(u, Option.empty[Document[Uml]])

  def getElementLocationOf
  (u: UMLElement[Uml],
   context: Option[Document[Uml]] = None)
  : ElementLocation
  = ds
    .fold[ElementLocation] {
    require(context.isEmpty)
    ElementLocation_ToolSpecific_ID_URL(u.toolSpecific_id, u.toolSpecific_url)
  } {
    _
      .lookupDocumentByExtent(u)
      .fold[ElementLocation] {
      ElementLocation_ToolSpecific_ID_URL(u.toolSpecific_id, u.toolSpecific_url)
    } { ud =>
      context
        .fold[ElementLocation] {
        ElementLocation_ToolSpecific_ID_OTI_URL(u.toolSpecific_id, ud.info.documentURL)
      } { d =>
        if (d == ud)
          ElementLocation_ToolSpecific_ID(u.toolSpecific_id)
        else
          ElementLocation_ToolSpecific_ID_OTI_URL(u.toolSpecific_id, ud.info.documentURL)
      }
    }
  }
  
  // <!-- End of user code additions -->

  def toOTIMOFElement
  (u: UMLElement[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = u match {
    case uu: UMLAcceptCallAction[Uml] => toOTIUMLAcceptCallAction(uu, context)
    case uu: UMLActionExecutionSpecification[Uml] => toOTIUMLActionExecutionSpecification(uu, context)
    case uu: UMLActionInputPin[Uml] => toOTIUMLActionInputPin(uu, context)
    case uu: UMLActivity[Uml] => toOTIUMLActivity(uu, context)
    case uu: UMLActivityFinalNode[Uml] => toOTIUMLActivityFinalNode(uu, context)
    case uu: UMLActivityParameterNode[Uml] => toOTIUMLActivityParameterNode(uu, context)
    case uu: UMLActivityPartition[Uml] => toOTIUMLActivityPartition(uu, context)
    case uu: UMLActor[Uml] => toOTIUMLActor(uu, context)
    case uu: UMLAddStructuralFeatureValueAction[Uml] => toOTIUMLAddStructuralFeatureValueAction(uu, context)
    case uu: UMLAddVariableValueAction[Uml] => toOTIUMLAddVariableValueAction(uu, context)
    case uu: UMLAnyReceiveEvent[Uml] => toOTIUMLAnyReceiveEvent(uu, context)
    case uu: UMLAssociationClass[Uml] => toOTIUMLAssociationClass(uu, context)
    case uu: UMLBehaviorExecutionSpecification[Uml] => toOTIUMLBehaviorExecutionSpecification(uu, context)
    case uu: UMLBroadcastSignalAction[Uml] => toOTIUMLBroadcastSignalAction(uu, context)
    case uu: UMLCallBehaviorAction[Uml] => toOTIUMLCallBehaviorAction(uu, context)
    case uu: UMLCallEvent[Uml] => toOTIUMLCallEvent(uu, context)
    case uu: UMLCallOperationAction[Uml] => toOTIUMLCallOperationAction(uu, context)
    case uu: UMLChangeEvent[Uml] => toOTIUMLChangeEvent(uu, context)
    case uu: UMLClassifierTemplateParameter[Uml] => toOTIUMLClassifierTemplateParameter(uu, context)
    case uu: UMLClause[Uml] => toOTIUMLClause(uu, context)
    case uu: UMLClearAssociationAction[Uml] => toOTIUMLClearAssociationAction(uu, context)
    case uu: UMLClearStructuralFeatureAction[Uml] => toOTIUMLClearStructuralFeatureAction(uu, context)
    case uu: UMLClearVariableAction[Uml] => toOTIUMLClearVariableAction(uu, context)
    case uu: UMLCollaboration[Uml] => toOTIUMLCollaboration(uu, context)
    case uu: UMLCollaborationUse[Uml] => toOTIUMLCollaborationUse(uu, context)
    case uu: UMLComment[Uml] => toOTIUMLComment(uu, context)
    case uu: UMLCommunicationPath[Uml] => toOTIUMLCommunicationPath(uu, context)
    case uu: UMLComponent[Uml] => toOTIUMLComponent(uu, context)
    case uu: UMLComponentRealization[Uml] => toOTIUMLComponentRealization(uu, context)
    case uu: UMLConditionalNode[Uml] => toOTIUMLConditionalNode(uu, context)
    case uu: UMLConnectableElementTemplateParameter[Uml] => toOTIUMLConnectableElementTemplateParameter(uu, context)
    case uu: UMLConnectionPointReference[Uml] => toOTIUMLConnectionPointReference(uu, context)
    case uu: UMLConnector[Uml] => toOTIUMLConnector(uu, context)
    case uu: UMLConnectorEnd[Uml] => toOTIUMLConnectorEnd(uu, context)
    case uu: UMLConsiderIgnoreFragment[Uml] => toOTIUMLConsiderIgnoreFragment(uu, context)
    case uu: UMLContinuation[Uml] => toOTIUMLContinuation(uu, context)
    case uu: UMLControlFlow[Uml] => toOTIUMLControlFlow(uu, context)
    case uu: UMLCreateLinkObjectAction[Uml] => toOTIUMLCreateLinkObjectAction(uu, context)
    case uu: UMLCreateObjectAction[Uml] => toOTIUMLCreateObjectAction(uu, context)
    case uu: UMLDataStoreNode[Uml] => toOTIUMLDataStoreNode(uu, context)
    case uu: UMLDecisionNode[Uml] => toOTIUMLDecisionNode(uu, context)
    case uu: UMLDeployment[Uml] => toOTIUMLDeployment(uu, context)
    case uu: UMLDeploymentSpecification[Uml] => toOTIUMLDeploymentSpecification(uu, context)
    case uu: UMLDestroyLinkAction[Uml] => toOTIUMLDestroyLinkAction(uu, context)
    case uu: UMLDestroyObjectAction[Uml] => toOTIUMLDestroyObjectAction(uu, context)
    case uu: UMLDestructionOccurrenceSpecification[Uml] => toOTIUMLDestructionOccurrenceSpecification(uu, context)
    case uu: UMLDevice[Uml] => toOTIUMLDevice(uu, context)
    case uu: UMLDuration[Uml] => toOTIUMLDuration(uu, context)
    case uu: UMLDurationConstraint[Uml] => toOTIUMLDurationConstraint(uu, context)
    case uu: UMLDurationInterval[Uml] => toOTIUMLDurationInterval(uu, context)
    case uu: UMLDurationObservation[Uml] => toOTIUMLDurationObservation(uu, context)
    case uu: UMLElementImport[Uml] => toOTIUMLElementImport(uu, context)
    case uu: UMLElementValue[Uml] => toOTIUMLElementValue(uu, context)
    case uu: UMLEnumeration[Uml] => toOTIUMLEnumeration(uu, context)
    case uu: UMLEnumerationLiteral[Uml] => toOTIUMLEnumerationLiteral(uu, context)
    case uu: UMLExceptionHandler[Uml] => toOTIUMLExceptionHandler(uu, context)
    case uu: UMLExecutionEnvironment[Uml] => toOTIUMLExecutionEnvironment(uu, context)
    case uu: UMLExecutionOccurrenceSpecification[Uml] => toOTIUMLExecutionOccurrenceSpecification(uu, context)
    case uu: UMLExpansionNode[Uml] => toOTIUMLExpansionNode(uu, context)
    case uu: UMLExpansionRegion[Uml] => toOTIUMLExpansionRegion(uu, context)
    case uu: UMLExtend[Uml] => toOTIUMLExtend(uu, context)
    case uu: UMLExtension[Uml] => toOTIUMLExtension(uu, context)
    case uu: UMLExtensionEnd[Uml] => toOTIUMLExtensionEnd(uu, context)
    case uu: UMLExtensionPoint[Uml] => toOTIUMLExtensionPoint(uu, context)
    case uu: UMLFinalState[Uml] => toOTIUMLFinalState(uu, context)
    case uu: UMLFlowFinalNode[Uml] => toOTIUMLFlowFinalNode(uu, context)
    case uu: UMLForkNode[Uml] => toOTIUMLForkNode(uu, context)
    case uu: UMLFunctionBehavior[Uml] => toOTIUMLFunctionBehavior(uu, context)
    case uu: UMLGate[Uml] => toOTIUMLGate(uu, context)
    case uu: UMLGeneralOrdering[Uml] => toOTIUMLGeneralOrdering(uu, context)
    case uu: UMLGeneralization[Uml] => toOTIUMLGeneralization(uu, context)
    case uu: UMLGeneralizationSet[Uml] => toOTIUMLGeneralizationSet(uu, context)
    case uu: UMLImage[Uml] => toOTIUMLImage(uu, context)
    case uu: UMLInclude[Uml] => toOTIUMLInclude(uu, context)
    case uu: UMLInformationFlow[Uml] => toOTIUMLInformationFlow(uu, context)
    case uu: UMLInformationItem[Uml] => toOTIUMLInformationItem(uu, context)
    case uu: UMLInitialNode[Uml] => toOTIUMLInitialNode(uu, context)
    case uu: UMLInstanceValue[Uml] => toOTIUMLInstanceValue(uu, context)
    case uu: UMLInteraction[Uml] => toOTIUMLInteraction(uu, context)
    case uu: UMLInteractionConstraint[Uml] => toOTIUMLInteractionConstraint(uu, context)
    case uu: UMLInteractionOperand[Uml] => toOTIUMLInteractionOperand(uu, context)
    case uu: UMLInterface[Uml] => toOTIUMLInterface(uu, context)
    case uu: UMLInterfaceRealization[Uml] => toOTIUMLInterfaceRealization(uu, context)
    case uu: UMLInterruptibleActivityRegion[Uml] => toOTIUMLInterruptibleActivityRegion(uu, context)
    case uu: UMLJoinNode[Uml] => toOTIUMLJoinNode(uu, context)
    case uu: UMLLifeline[Uml] => toOTIUMLLifeline(uu, context)
    case uu: UMLLinkEndCreationData[Uml] => toOTIUMLLinkEndCreationData(uu, context)
    case uu: UMLLinkEndDestructionData[Uml] => toOTIUMLLinkEndDestructionData(uu, context)
    case uu: UMLLiteralBoolean[Uml] => toOTIUMLLiteralBoolean(uu, context)
    case uu: UMLLiteralInteger[Uml] => toOTIUMLLiteralInteger(uu, context)
    case uu: UMLLiteralNull[Uml] => toOTIUMLLiteralNull(uu, context)
    case uu: UMLLiteralReal[Uml] => toOTIUMLLiteralReal(uu, context)
    case uu: UMLLiteralString[Uml] => toOTIUMLLiteralString(uu, context)
    case uu: UMLLiteralUnlimitedNatural[Uml] => toOTIUMLLiteralUnlimitedNatural(uu, context)
    case uu: UMLLoopNode[Uml] => toOTIUMLLoopNode(uu, context)
    case uu: UMLManifestation[Uml] => toOTIUMLManifestation(uu, context)
    case uu: UMLMergeNode[Uml] => toOTIUMLMergeNode(uu, context)
    case uu: UMLMessage[Uml] => toOTIUMLMessage(uu, context)
    case uu: UMLModel[Uml] => toOTIUMLModel(uu, context)
    case uu: UMLObjectFlow[Uml] => toOTIUMLObjectFlow(uu, context)
    case uu: UMLOpaqueAction[Uml] => toOTIUMLOpaqueAction(uu, context)
    case uu: UMLOpaqueExpression[Uml] => toOTIUMLOpaqueExpression(uu, context)
    case uu: UMLOperation[Uml] => toOTIUMLOperation(uu, context)
    case uu: UMLOperationTemplateParameter[Uml] => toOTIUMLOperationTemplateParameter(uu, context)
    case uu: UMLOutputPin[Uml] => toOTIUMLOutputPin(uu, context)
    case uu: UMLPackageImport[Uml] => toOTIUMLPackageImport(uu, context)
    case uu: UMLPackageMerge[Uml] => toOTIUMLPackageMerge(uu, context)
    case uu: UMLParameter[Uml] => toOTIUMLParameter(uu, context)
    case uu: UMLParameterSet[Uml] => toOTIUMLParameterSet(uu, context)
    case uu: UMLPartDecomposition[Uml] => toOTIUMLPartDecomposition(uu, context)
    case uu: UMLPort[Uml] => toOTIUMLPort(uu, context)
    case uu: UMLPrimitiveType[Uml] => toOTIUMLPrimitiveType(uu, context)
    case uu: UMLProfile[Uml] => toOTIUMLProfile(uu, context)
    case uu: UMLProfileApplication[Uml] => toOTIUMLProfileApplication(uu, context)
    case uu: UMLProtocolConformance[Uml] => toOTIUMLProtocolConformance(uu, context)
    case uu: UMLProtocolStateMachine[Uml] => toOTIUMLProtocolStateMachine(uu, context)
    case uu: UMLProtocolTransition[Uml] => toOTIUMLProtocolTransition(uu, context)
    case uu: UMLPseudostate[Uml] => toOTIUMLPseudostate(uu, context)
    case uu: UMLQualifierValue[Uml] => toOTIUMLQualifierValue(uu, context)
    case uu: UMLRaiseExceptionAction[Uml] => toOTIUMLRaiseExceptionAction(uu, context)
    case uu: UMLReadExtentAction[Uml] => toOTIUMLReadExtentAction(uu, context)
    case uu: UMLReadIsClassifiedObjectAction[Uml] => toOTIUMLReadIsClassifiedObjectAction(uu, context)
    case uu: UMLReadLinkAction[Uml] => toOTIUMLReadLinkAction(uu, context)
    case uu: UMLReadLinkObjectEndAction[Uml] => toOTIUMLReadLinkObjectEndAction(uu, context)
    case uu: UMLReadLinkObjectEndQualifierAction[Uml] => toOTIUMLReadLinkObjectEndQualifierAction(uu, context)
    case uu: UMLReadSelfAction[Uml] => toOTIUMLReadSelfAction(uu, context)
    case uu: UMLReadStructuralFeatureAction[Uml] => toOTIUMLReadStructuralFeatureAction(uu, context)
    case uu: UMLReadVariableAction[Uml] => toOTIUMLReadVariableAction(uu, context)
    case uu: UMLReception[Uml] => toOTIUMLReception(uu, context)
    case uu: UMLReclassifyObjectAction[Uml] => toOTIUMLReclassifyObjectAction(uu, context)
    case uu: UMLRedefinableTemplateSignature[Uml] => toOTIUMLRedefinableTemplateSignature(uu, context)
    case uu: UMLReduceAction[Uml] => toOTIUMLReduceAction(uu, context)
    case uu: UMLRegion[Uml] => toOTIUMLRegion(uu, context)
    case uu: UMLRemoveStructuralFeatureValueAction[Uml] => toOTIUMLRemoveStructuralFeatureValueAction(uu, context)
    case uu: UMLRemoveVariableValueAction[Uml] => toOTIUMLRemoveVariableValueAction(uu, context)
    case uu: UMLReplyAction[Uml] => toOTIUMLReplyAction(uu, context)
    case uu: UMLSendObjectAction[Uml] => toOTIUMLSendObjectAction(uu, context)
    case uu: UMLSendSignalAction[Uml] => toOTIUMLSendSignalAction(uu, context)
    case uu: UMLSequenceNode[Uml] => toOTIUMLSequenceNode(uu, context)
    case uu: UMLSignal[Uml] => toOTIUMLSignal(uu, context)
    case uu: UMLSignalEvent[Uml] => toOTIUMLSignalEvent(uu, context)
    case uu: UMLSlot[Uml] => toOTIUMLSlot(uu, context)
    case uu: UMLStartClassifierBehaviorAction[Uml] => toOTIUMLStartClassifierBehaviorAction(uu, context)
    case uu: UMLStartObjectBehaviorAction[Uml] => toOTIUMLStartObjectBehaviorAction(uu, context)
    case uu: UMLStateInvariant[Uml] => toOTIUMLStateInvariant(uu, context)
    case uu: UMLStereotype[Uml] => toOTIUMLStereotype(uu, context)
    case uu: UMLStringExpression[Uml] => toOTIUMLStringExpression(uu, context)
    case uu: UMLSubstitution[Uml] => toOTIUMLSubstitution(uu, context)
    case uu: UMLTemplateBinding[Uml] => toOTIUMLTemplateBinding(uu, context)
    case uu: UMLTemplateParameterSubstitution[Uml] => toOTIUMLTemplateParameterSubstitution(uu, context)
    case uu: UMLTestIdentityAction[Uml] => toOTIUMLTestIdentityAction(uu, context)
    case uu: UMLTimeConstraint[Uml] => toOTIUMLTimeConstraint(uu, context)
    case uu: UMLTimeEvent[Uml] => toOTIUMLTimeEvent(uu, context)
    case uu: UMLTimeExpression[Uml] => toOTIUMLTimeExpression(uu, context)
    case uu: UMLTimeInterval[Uml] => toOTIUMLTimeInterval(uu, context)
    case uu: UMLTimeObservation[Uml] => toOTIUMLTimeObservation(uu, context)
    case uu: UMLTrigger[Uml] => toOTIUMLTrigger(uu, context)
    case uu: UMLUnmarshallAction[Uml] => toOTIUMLUnmarshallAction(uu, context)
    case uu: UMLUsage[Uml] => toOTIUMLUsage(uu, context)
    case uu: UMLUseCase[Uml] => toOTIUMLUseCase(uu, context)
    case uu: UMLValuePin[Uml] => toOTIUMLValuePin(uu, context)
    case uu: UMLValueSpecificationAction[Uml] => toOTIUMLValueSpecificationAction(uu, context)
    case uu: UMLVariable[Uml] => toOTIUMLVariable(uu, context)
    case uu: UMLAcceptEventAction[Uml] => toOTIUMLAcceptEventAction(uu, context)
    case uu: UMLArtifact[Uml] => toOTIUMLArtifact(uu, context)
    case uu: UMLAssociation[Uml] => toOTIUMLAssociation(uu, context)
    case uu: UMLCentralBufferNode[Uml] => toOTIUMLCentralBufferNode(uu, context)
    case uu: UMLCombinedFragment[Uml] => toOTIUMLCombinedFragment(uu, context)
    case uu: UMLCreateLinkAction[Uml] => toOTIUMLCreateLinkAction(uu, context)
    case uu: UMLDataType[Uml] => toOTIUMLDataType(uu, context)
    case uu: UMLExpression[Uml] => toOTIUMLExpression(uu, context)
    case uu: UMLInputPin[Uml] => toOTIUMLInputPin(uu, context)
    case uu: UMLInstanceSpecification[Uml] => toOTIUMLInstanceSpecification(uu, context)
    case uu: UMLInteractionUse[Uml] => toOTIUMLInteractionUse(uu, context)
    case uu: UMLInterval[Uml] => toOTIUMLInterval(uu, context)
    case uu: UMLIntervalConstraint[Uml] => toOTIUMLIntervalConstraint(uu, context)
    case uu: UMLLinkEndData[Uml] => toOTIUMLLinkEndData(uu, context)
    case uu: UMLMessageOccurrenceSpecification[Uml] => toOTIUMLMessageOccurrenceSpecification(uu, context)
    case uu: UMLNode[Uml] => toOTIUMLNode(uu, context)
    case uu: UMLOpaqueBehavior[Uml] => toOTIUMLOpaqueBehavior(uu, context)
    case uu: UMLPackage[Uml] => toOTIUMLPackage(uu, context)
    case uu: UMLProperty[Uml] => toOTIUMLProperty(uu, context)
    case uu: UMLRealization[Uml] => toOTIUMLRealization(uu, context)
    case uu: UMLState[Uml] => toOTIUMLState(uu, context)
    case uu: UMLStateMachine[Uml] => toOTIUMLStateMachine(uu, context)
    case uu: UMLStructuredActivityNode[Uml] => toOTIUMLStructuredActivityNode(uu, context)
    case uu: UMLTemplateParameter[Uml] => toOTIUMLTemplateParameter(uu, context)
    case uu: UMLTemplateSignature[Uml] => toOTIUMLTemplateSignature(uu, context)
    case uu: UMLTransition[Uml] => toOTIUMLTransition(uu, context)
    case uu: UMLAbstraction[Uml] => toOTIUMLAbstraction(uu, context)
    case uu: UMLClass[Uml] => toOTIUMLClass(uu, context)
    case uu: UMLConstraint[Uml] => toOTIUMLConstraint(uu, context)
    case uu: UMLOccurrenceSpecification[Uml] => toOTIUMLOccurrenceSpecification(uu, context)
    case uu: UMLDependency[Uml] => toOTIUMLDependency(uu, context)
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

  def toOTIUMLAbstraction
  (u: UMLAbstraction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLAbstraction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLAcceptCallAction
  (u: UMLAcceptCallAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLAcceptCallAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isUnmarshall = u.isUnmarshall,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLAcceptEventAction
  (u: UMLAcceptEventAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLAcceptEventAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isUnmarshall = u.isUnmarshall,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLActionExecutionSpecification
  (u: UMLActionExecutionSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLActionExecutionSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLActionInputPin
  (u: UMLActionInputPin[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLActionInputPin(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControl = u.isControl,
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     isOrdered = u.isOrdered,
     isUnique = u.isUnique,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)

  def toOTIUMLActivity
  (u: UMLActivity[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLActivity(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     isReadOnly = u.isReadOnly,
     isReentrant = u.isReentrant,
     isSingleExecution = u.isSingleExecution,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLActivityFinalNode
  (u: UMLActivityFinalNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLActivityFinalNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLActivityParameterNode
  (u: UMLActivityParameterNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLActivityParameterNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)

  def toOTIUMLActivityPartition
  (u: UMLActivityPartition[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLActivityPartition(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isDimension = u.isDimension,
     isExternal = u.isExternal,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLActor
  (u: UMLActor[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLActor(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLAddStructuralFeatureValueAction
  (u: UMLAddStructuralFeatureValueAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLAddStructuralFeatureValueAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isReplaceAll = u.isReplaceAll,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLAddVariableValueAction
  (u: UMLAddVariableValueAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLAddVariableValueAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isReplaceAll = u.isReplaceAll,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLAnyReceiveEvent
  (u: UMLAnyReceiveEvent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLAnyReceiveEvent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLArtifact
  (u: UMLArtifact[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLArtifact(
     otiMOFElementLocation = getElementLocationOf(u, context),
     fileName = u.fileName,
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLAssociation
  (u: UMLAssociation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLAssociation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isDerived = u.isDerived,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLAssociationClass
  (u: UMLAssociationClass[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLAssociationClass(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isDerived = u.isDerived,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLBehaviorExecutionSpecification
  (u: UMLBehaviorExecutionSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLBehaviorExecutionSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLBroadcastSignalAction
  (u: UMLBroadcastSignalAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLBroadcastSignalAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLCallBehaviorAction
  (u: UMLCallBehaviorAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLCallBehaviorAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isSynchronous = u.isSynchronous,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLCallEvent
  (u: UMLCallEvent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLCallEvent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLCallOperationAction
  (u: UMLCallOperationAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLCallOperationAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isSynchronous = u.isSynchronous,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLCentralBufferNode
  (u: UMLCentralBufferNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLCentralBufferNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)

  def toOTIUMLChangeEvent
  (u: UMLChangeEvent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLChangeEvent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLClass
  (u: UMLClass[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLClass(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLClassifierTemplateParameter
  (u: UMLClassifierTemplateParameter[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLClassifierTemplateParameter(
     otiMOFElementLocation = getElementLocationOf(u, context),
     allowSubstitutable = u.allowSubstitutable)

  def toOTIUMLClause
  (u: UMLClause[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLClause(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLClearAssociationAction
  (u: UMLClearAssociationAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLClearAssociationAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLClearStructuralFeatureAction
  (u: UMLClearStructuralFeatureAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLClearStructuralFeatureAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLClearVariableAction
  (u: UMLClearVariableAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLClearVariableAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLCollaboration
  (u: UMLCollaboration[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLCollaboration(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLCollaborationUse
  (u: UMLCollaborationUse[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLCollaborationUse(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLCombinedFragment
  (u: UMLCombinedFragment[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLCombinedFragment(
     otiMOFElementLocation = getElementLocationOf(u, context),
     interactionOperator = u.interactionOperator,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLComment
  (u: UMLComment[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLComment(
     otiMOFElementLocation = getElementLocationOf(u, context),
     body = u.body)

  def toOTIUMLCommunicationPath
  (u: UMLCommunicationPath[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLCommunicationPath(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isDerived = u.isDerived,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLComponent
  (u: UMLComponent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLComponent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isIndirectlyInstantiated = u.isIndirectlyInstantiated,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLComponentRealization
  (u: UMLComponentRealization[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLComponentRealization(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLConditionalNode
  (u: UMLConditionalNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLConditionalNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAssured = u.isAssured,
     isDeterminate = u.isDeterminate,
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     mustIsolate = u.mustIsolate,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLConnectableElementTemplateParameter
  (u: UMLConnectableElementTemplateParameter[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLConnectableElementTemplateParameter(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLConnectionPointReference
  (u: UMLConnectionPointReference[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLConnectionPointReference(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLConnector
  (u: UMLConnector[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLConnector(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isStatic = u.isStatic,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLConnectorEnd
  (u: UMLConnectorEnd[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLConnectorEnd(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isOrdered = u.isOrdered,
     isUnique = u.isUnique)

  def toOTIUMLConsiderIgnoreFragment
  (u: UMLConsiderIgnoreFragment[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLConsiderIgnoreFragment(
     otiMOFElementLocation = getElementLocationOf(u, context),
     interactionOperator = u.interactionOperator,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLConstraint
  (u: UMLConstraint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLConstraint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLContinuation
  (u: UMLContinuation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLContinuation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     setting = u.setting,
     visibility = u.visibility)

  def toOTIUMLControlFlow
  (u: UMLControlFlow[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLControlFlow(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLCreateLinkAction
  (u: UMLCreateLinkAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLCreateLinkAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLCreateLinkObjectAction
  (u: UMLCreateLinkObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLCreateLinkObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLCreateObjectAction
  (u: UMLCreateObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLCreateObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDataStoreNode
  (u: UMLDataStoreNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDataStoreNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)

  def toOTIUMLDataType
  (u: UMLDataType[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDataType(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDecisionNode
  (u: UMLDecisionNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDecisionNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDependency
  (u: UMLDependency[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDependency(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDeployment
  (u: UMLDeployment[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDeployment(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDeploymentSpecification
  (u: UMLDeploymentSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDeploymentSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     deploymentLocation = u.deploymentLocation,
     executionLocation = u.executionLocation,
     fileName = u.fileName,
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDestroyLinkAction
  (u: UMLDestroyLinkAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDestroyLinkAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDestroyObjectAction
  (u: UMLDestroyObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDestroyObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isDestroyLinks = u.isDestroyLinks,
     isDestroyOwnedObjects = u.isDestroyOwnedObjects,
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDestructionOccurrenceSpecification
  (u: UMLDestructionOccurrenceSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDestructionOccurrenceSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDevice
  (u: UMLDevice[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDevice(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDuration
  (u: UMLDuration[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDuration(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDurationConstraint
  (u: UMLDurationConstraint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDurationConstraint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     firstEvent = u.firstEvent,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDurationInterval
  (u: UMLDurationInterval[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDurationInterval(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLDurationObservation
  (u: UMLDurationObservation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLDurationObservation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     firstEvent = u.firstEvent,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLElementImport
  (u: UMLElementImport[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLElementImport(
     otiMOFElementLocation = getElementLocationOf(u, context),
     alias = u.alias,
     visibility = u.visibility)

  def toOTIUMLElementValue
  (u: UMLElementValue[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLElementValue(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLEnumeration
  (u: UMLEnumeration[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLEnumeration(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLEnumerationLiteral
  (u: UMLEnumerationLiteral[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLEnumerationLiteral(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLExceptionHandler
  (u: UMLExceptionHandler[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLExceptionHandler(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLExecutionEnvironment
  (u: UMLExecutionEnvironment[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLExecutionEnvironment(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLExecutionOccurrenceSpecification
  (u: UMLExecutionOccurrenceSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLExecutionOccurrenceSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLExpansionNode
  (u: UMLExpansionNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLExpansionNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)

  def toOTIUMLExpansionRegion
  (u: UMLExpansionRegion[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLExpansionRegion(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     mode = u.mode,
     mustIsolate = u.mustIsolate,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLExpression
  (u: UMLExpression[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLExpression(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     symbol = u.symbol,
     visibility = u.visibility)

  def toOTIUMLExtend
  (u: UMLExtend[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLExtend(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLExtension
  (u: UMLExtension[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLExtension(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isDerived = u.isDerived,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLExtensionEnd
  (u: UMLExtensionEnd[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLExtensionEnd(
     otiMOFElementLocation = getElementLocationOf(u, context),
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
     visibility = u.visibility)

  def toOTIUMLExtensionPoint
  (u: UMLExtensionPoint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLExtensionPoint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLFinalState
  (u: UMLFinalState[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLFinalState(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLFlowFinalNode
  (u: UMLFlowFinalNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLFlowFinalNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLForkNode
  (u: UMLForkNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLForkNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLFunctionBehavior
  (u: UMLFunctionBehavior[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLFunctionBehavior(
     otiMOFElementLocation = getElementLocationOf(u, context),
     body = u.body,
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     isReentrant = u.isReentrant,
     language = u.language,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLGate
  (u: UMLGate[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLGate(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLGeneralOrdering
  (u: UMLGeneralOrdering[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLGeneralOrdering(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLGeneralization
  (u: UMLGeneralization[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLGeneralization(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isSubstitutable = u.isSubstitutable)

  def toOTIUMLGeneralizationSet
  (u: UMLGeneralizationSet[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLGeneralizationSet(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isCovering = u.isCovering,
     isDisjoint = u.isDisjoint,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLImage
  (u: UMLImage[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLImage(
     otiMOFElementLocation = getElementLocationOf(u, context),
     content = u.content,
     format = u.format,
     location = u.location)

  def toOTIUMLInclude
  (u: UMLInclude[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInclude(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInformationFlow
  (u: UMLInformationFlow[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInformationFlow(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInformationItem
  (u: UMLInformationItem[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInformationItem(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInitialNode
  (u: UMLInitialNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInitialNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInputPin
  (u: UMLInputPin[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInputPin(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControl = u.isControl,
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     isOrdered = u.isOrdered,
     isUnique = u.isUnique,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)

  def toOTIUMLInstanceSpecification
  (u: UMLInstanceSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInstanceSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInstanceValue
  (u: UMLInstanceValue[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInstanceValue(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInteraction
  (u: UMLInteraction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInteraction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     isReentrant = u.isReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInteractionConstraint
  (u: UMLInteractionConstraint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInteractionConstraint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInteractionOperand
  (u: UMLInteractionOperand[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInteractionOperand(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInteractionUse
  (u: UMLInteractionUse[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInteractionUse(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInterface
  (u: UMLInterface[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInterface(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInterfaceRealization
  (u: UMLInterfaceRealization[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInterfaceRealization(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInterruptibleActivityRegion
  (u: UMLInterruptibleActivityRegion[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInterruptibleActivityRegion(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLInterval
  (u: UMLInterval[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLInterval(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLIntervalConstraint
  (u: UMLIntervalConstraint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLIntervalConstraint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLJoinNode
  (u: UMLJoinNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLJoinNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isCombineDuplicate = u.isCombineDuplicate,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLLifeline
  (u: UMLLifeline[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLLifeline(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLLinkEndCreationData
  (u: UMLLinkEndCreationData[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLLinkEndCreationData(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isReplaceAll = u.isReplaceAll)

  def toOTIUMLLinkEndData
  (u: UMLLinkEndData[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLLinkEndData(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLLinkEndDestructionData
  (u: UMLLinkEndDestructionData[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLLinkEndDestructionData(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isDestroyDuplicates = u.isDestroyDuplicates)

  def toOTIUMLLiteralBoolean
  (u: UMLLiteralBoolean[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLLiteralBoolean(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     value = u.value,
     visibility = u.visibility)

  def toOTIUMLLiteralInteger
  (u: UMLLiteralInteger[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLLiteralInteger(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     value = u.value,
     visibility = u.visibility)

  def toOTIUMLLiteralNull
  (u: UMLLiteralNull[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLLiteralNull(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLLiteralReal
  (u: UMLLiteralReal[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLLiteralReal(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     value = u.value,
     visibility = u.visibility)

  def toOTIUMLLiteralString
  (u: UMLLiteralString[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLLiteralString(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     value = u.value,
     visibility = u.visibility)

  def toOTIUMLLiteralUnlimitedNatural
  (u: UMLLiteralUnlimitedNatural[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLLiteralUnlimitedNatural(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     value = u.value,
     visibility = u.visibility)

  def toOTIUMLLoopNode
  (u: UMLLoopNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLLoopNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isTestedFirst = u.isTestedFirst,
     mustIsolate = u.mustIsolate,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLManifestation
  (u: UMLManifestation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLManifestation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLMergeNode
  (u: UMLMergeNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLMergeNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLMessage
  (u: UMLMessage[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLMessage(
     otiMOFElementLocation = getElementLocationOf(u, context),
     messageSort = u.messageSort,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLMessageOccurrenceSpecification
  (u: UMLMessageOccurrenceSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLMessageOccurrenceSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLModel
  (u: UMLModel[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLModel(
     otiMOFElementLocation = getElementLocationOf(u, context),
     URI = u.URI,
     name = u.name,
     viewpoint = u.viewpoint,
     visibility = u.visibility)

  def toOTIUMLNode
  (u: UMLNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLObjectFlow
  (u: UMLObjectFlow[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLObjectFlow(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isMulticast = u.isMulticast,
     isMultireceive = u.isMultireceive,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLOccurrenceSpecification
  (u: UMLOccurrenceSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLOccurrenceSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLOpaqueAction
  (u: UMLOpaqueAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLOpaqueAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     body = u.body,
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     language = u.language,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLOpaqueBehavior
  (u: UMLOpaqueBehavior[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLOpaqueBehavior(
     otiMOFElementLocation = getElementLocationOf(u, context),
     body = u.body,
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     isReentrant = u.isReentrant,
     language = u.language,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLOpaqueExpression
  (u: UMLOpaqueExpression[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLOpaqueExpression(
     otiMOFElementLocation = getElementLocationOf(u, context),
     body = u.body,
     language = u.language,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLOperation
  (u: UMLOperation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLOperation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     concurrency = u.concurrency,
     isAbstract = u.isAbstract,
     isLeaf = u.isLeaf,
     isQuery = u.isQuery,
     isStatic = u.isStatic,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLOperationTemplateParameter
  (u: UMLOperationTemplateParameter[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLOperationTemplateParameter(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLOutputPin
  (u: UMLOutputPin[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLOutputPin(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControl = u.isControl,
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     isOrdered = u.isOrdered,
     isUnique = u.isUnique,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)

  def toOTIUMLPackage
  (u: UMLPackage[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLPackage(
     otiMOFElementLocation = getElementLocationOf(u, context),
     URI = u.URI,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLPackageImport
  (u: UMLPackageImport[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLPackageImport(
     otiMOFElementLocation = getElementLocationOf(u, context),
     visibility = u.visibility)

  def toOTIUMLPackageMerge
  (u: UMLPackageMerge[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLPackageMerge(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLParameter
  (u: UMLParameter[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLParameter(
     otiMOFElementLocation = getElementLocationOf(u, context),
     direction = u.direction,
     effect = u.effect,
     isException = u.isException,
     isOrdered = u.isOrdered,
     isStream = u.isStream,
     isUnique = u.isUnique,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLParameterSet
  (u: UMLParameterSet[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLParameterSet(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLPartDecomposition
  (u: UMLPartDecomposition[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLPartDecomposition(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLPort
  (u: UMLPort[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLPort(
     otiMOFElementLocation = getElementLocationOf(u, context),
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
     visibility = u.visibility)

  def toOTIUMLPrimitiveType
  (u: UMLPrimitiveType[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLPrimitiveType(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLProfile
  (u: UMLProfile[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLProfile(
     otiMOFElementLocation = getElementLocationOf(u, context),
     URI = u.URI,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLProfileApplication
  (u: UMLProfileApplication[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLProfileApplication(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isStrict = u.isStrict)

  def toOTIUMLProperty
  (u: UMLProperty[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLProperty(
     otiMOFElementLocation = getElementLocationOf(u, context),
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
     visibility = u.visibility)

  def toOTIUMLProtocolConformance
  (u: UMLProtocolConformance[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLProtocolConformance(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLProtocolStateMachine
  (u: UMLProtocolStateMachine[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLProtocolStateMachine(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     isReentrant = u.isReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLProtocolTransition
  (u: UMLProtocolTransition[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLProtocolTransition(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     kind = u.kind,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLPseudostate
  (u: UMLPseudostate[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLPseudostate(
     otiMOFElementLocation = getElementLocationOf(u, context),
     kind = u.kind,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLQualifierValue
  (u: UMLQualifierValue[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLQualifierValue(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLRaiseExceptionAction
  (u: UMLRaiseExceptionAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLRaiseExceptionAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReadExtentAction
  (u: UMLReadExtentAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReadExtentAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReadIsClassifiedObjectAction
  (u: UMLReadIsClassifiedObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReadIsClassifiedObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isDirect = u.isDirect,
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReadLinkAction
  (u: UMLReadLinkAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReadLinkAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReadLinkObjectEndAction
  (u: UMLReadLinkObjectEndAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReadLinkObjectEndAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReadLinkObjectEndQualifierAction
  (u: UMLReadLinkObjectEndQualifierAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReadLinkObjectEndQualifierAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReadSelfAction
  (u: UMLReadSelfAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReadSelfAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReadStructuralFeatureAction
  (u: UMLReadStructuralFeatureAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReadStructuralFeatureAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReadVariableAction
  (u: UMLReadVariableAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReadVariableAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLRealization
  (u: UMLRealization[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLRealization(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReception
  (u: UMLReception[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReception(
     otiMOFElementLocation = getElementLocationOf(u, context),
     concurrency = u.concurrency,
     isAbstract = u.isAbstract,
     isLeaf = u.isLeaf,
     isStatic = u.isStatic,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReclassifyObjectAction
  (u: UMLReclassifyObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReclassifyObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isReplaceAll = u.isReplaceAll,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLRedefinableTemplateSignature
  (u: UMLRedefinableTemplateSignature[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLRedefinableTemplateSignature(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReduceAction
  (u: UMLReduceAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReduceAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isOrdered = u.isOrdered,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLRegion
  (u: UMLRegion[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLRegion(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLRemoveStructuralFeatureValueAction
  (u: UMLRemoveStructuralFeatureValueAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLRemoveStructuralFeatureValueAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isRemoveDuplicates = u.isRemoveDuplicates,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLRemoveVariableValueAction
  (u: UMLRemoveVariableValueAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLRemoveVariableValueAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isRemoveDuplicates = u.isRemoveDuplicates,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLReplyAction
  (u: UMLReplyAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLReplyAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLSendObjectAction
  (u: UMLSendObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLSendObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLSendSignalAction
  (u: UMLSendSignalAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLSendSignalAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLSequenceNode
  (u: UMLSequenceNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLSequenceNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     mustIsolate = u.mustIsolate,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLSignal
  (u: UMLSignal[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLSignal(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLSignalEvent
  (u: UMLSignalEvent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLSignalEvent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLSlot
  (u: UMLSlot[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLSlot(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLStartClassifierBehaviorAction
  (u: UMLStartClassifierBehaviorAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLStartClassifierBehaviorAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLStartObjectBehaviorAction
  (u: UMLStartObjectBehaviorAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLStartObjectBehaviorAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isSynchronous = u.isSynchronous,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLState
  (u: UMLState[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLState(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLStateInvariant
  (u: UMLStateInvariant[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLStateInvariant(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLStateMachine
  (u: UMLStateMachine[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLStateMachine(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     isReentrant = u.isReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLStereotype
  (u: UMLStereotype[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLStereotype(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLStringExpression
  (u: UMLStringExpression[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLStringExpression(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     symbol = u.symbol,
     visibility = u.visibility)

  def toOTIUMLStructuredActivityNode
  (u: UMLStructuredActivityNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLStructuredActivityNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     mustIsolate = u.mustIsolate,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLSubstitution
  (u: UMLSubstitution[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLSubstitution(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLTemplateBinding
  (u: UMLTemplateBinding[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTemplateBinding(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLTemplateParameter
  (u: UMLTemplateParameter[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTemplateParameter(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLTemplateParameterSubstitution
  (u: UMLTemplateParameterSubstitution[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTemplateParameterSubstitution(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLTemplateSignature
  (u: UMLTemplateSignature[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTemplateSignature(
     otiMOFElementLocation = getElementLocationOf(u, context))

  def toOTIUMLTestIdentityAction
  (u: UMLTestIdentityAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTestIdentityAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLTimeConstraint
  (u: UMLTimeConstraint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTimeConstraint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     firstEvent = u.firstEvent,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLTimeEvent
  (u: UMLTimeEvent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTimeEvent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isRelative = u.isRelative,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLTimeExpression
  (u: UMLTimeExpression[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTimeExpression(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLTimeInterval
  (u: UMLTimeInterval[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTimeInterval(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLTimeObservation
  (u: UMLTimeObservation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTimeObservation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     firstEvent = u.firstEvent,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLTransition
  (u: UMLTransition[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTransition(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     kind = u.kind,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLTrigger
  (u: UMLTrigger[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLTrigger(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLUnmarshallAction
  (u: UMLUnmarshallAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLUnmarshallAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLUsage
  (u: UMLUsage[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLUsage(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLUseCase
  (u: UMLUseCase[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLUseCase(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLValuePin
  (u: UMLValuePin[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLValuePin(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControl = u.isControl,
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     isOrdered = u.isOrdered,
     isUnique = u.isUnique,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)

  def toOTIUMLValueSpecificationAction
  (u: UMLValueSpecificationAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLValueSpecificationAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)

  def toOTIUMLVariable
  (u: UMLVariable[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement
  = OTIMOFElement.OTIUMLVariable(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isOrdered = u.isOrdered,
     isUnique = u.isUnique,
     name = u.name,
     visibility = u.visibility)
}