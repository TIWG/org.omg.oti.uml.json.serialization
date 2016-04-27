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

import scala.{Boolean,Double,Int,Option,None}
import scala.Predef.{require,Integer2int}
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
    case uu: UMLDiagram[Uml] => toOTIUMLDiagram(uu, context)
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

  import oa.umlOps._

  implicit def toOTIUMLAbstraction
  (u: Uml#Abstraction)
  : OTIMOFElement.OTIUMLAbstraction
  = toOTIUMLAbstraction(u, Option.empty[Document[Uml]])

  def toOTIUMLAbstraction
  (u: UMLAbstraction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLAbstraction
  = OTIMOFElement.OTIUMLAbstraction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLAcceptCallAction
  (u: Uml#AcceptCallAction)
  : OTIMOFElement.OTIUMLAcceptCallAction
  = toOTIUMLAcceptCallAction(u, Option.empty[Document[Uml]])

  def toOTIUMLAcceptCallAction
  (u: UMLAcceptCallAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLAcceptCallAction
  = OTIMOFElement.OTIUMLAcceptCallAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isUnmarshall = u.isUnmarshall,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLAcceptEventAction
  (u: Uml#AcceptEventAction)
  : OTIMOFElement.OTIUMLAcceptEventAction
  = toOTIUMLAcceptEventAction(u, Option.empty[Document[Uml]])

  def toOTIUMLAcceptEventAction
  (u: UMLAcceptEventAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLAcceptEventAction
  = OTIMOFElement.OTIUMLAcceptEventAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isUnmarshall = u.isUnmarshall,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLActionExecutionSpecification
  (u: Uml#ActionExecutionSpecification)
  : OTIMOFElement.OTIUMLActionExecutionSpecification
  = toOTIUMLActionExecutionSpecification(u, Option.empty[Document[Uml]])

  def toOTIUMLActionExecutionSpecification
  (u: UMLActionExecutionSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLActionExecutionSpecification
  = OTIMOFElement.OTIUMLActionExecutionSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLActionInputPin
  (u: Uml#ActionInputPin)
  : OTIMOFElement.OTIUMLActionInputPin
  = toOTIUMLActionInputPin(u, Option.empty[Document[Uml]])

  def toOTIUMLActionInputPin
  (u: UMLActionInputPin[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLActionInputPin
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


  implicit def toOTIUMLActivity
  (u: Uml#Activity)
  : OTIMOFElement.OTIUMLActivity
  = toOTIUMLActivity(u, Option.empty[Document[Uml]])

  def toOTIUMLActivity
  (u: UMLActivity[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLActivity
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


  implicit def toOTIUMLActivityFinalNode
  (u: Uml#ActivityFinalNode)
  : OTIMOFElement.OTIUMLActivityFinalNode
  = toOTIUMLActivityFinalNode(u, Option.empty[Document[Uml]])

  def toOTIUMLActivityFinalNode
  (u: UMLActivityFinalNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLActivityFinalNode
  = OTIMOFElement.OTIUMLActivityFinalNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLActivityParameterNode
  (u: Uml#ActivityParameterNode)
  : OTIMOFElement.OTIUMLActivityParameterNode
  = toOTIUMLActivityParameterNode(u, Option.empty[Document[Uml]])

  def toOTIUMLActivityParameterNode
  (u: UMLActivityParameterNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLActivityParameterNode
  = OTIMOFElement.OTIUMLActivityParameterNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)


  implicit def toOTIUMLActivityPartition
  (u: Uml#ActivityPartition)
  : OTIMOFElement.OTIUMLActivityPartition
  = toOTIUMLActivityPartition(u, Option.empty[Document[Uml]])

  def toOTIUMLActivityPartition
  (u: UMLActivityPartition[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLActivityPartition
  = OTIMOFElement.OTIUMLActivityPartition(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isDimension = u.isDimension,
     isExternal = u.isExternal,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLActor
  (u: Uml#Actor)
  : OTIMOFElement.OTIUMLActor
  = toOTIUMLActor(u, Option.empty[Document[Uml]])

  def toOTIUMLActor
  (u: UMLActor[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLActor
  = OTIMOFElement.OTIUMLActor(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLAddStructuralFeatureValueAction
  (u: Uml#AddStructuralFeatureValueAction)
  : OTIMOFElement.OTIUMLAddStructuralFeatureValueAction
  = toOTIUMLAddStructuralFeatureValueAction(u, Option.empty[Document[Uml]])

  def toOTIUMLAddStructuralFeatureValueAction
  (u: UMLAddStructuralFeatureValueAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLAddStructuralFeatureValueAction
  = OTIMOFElement.OTIUMLAddStructuralFeatureValueAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isReplaceAll = u.isReplaceAll,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLAddVariableValueAction
  (u: Uml#AddVariableValueAction)
  : OTIMOFElement.OTIUMLAddVariableValueAction
  = toOTIUMLAddVariableValueAction(u, Option.empty[Document[Uml]])

  def toOTIUMLAddVariableValueAction
  (u: UMLAddVariableValueAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLAddVariableValueAction
  = OTIMOFElement.OTIUMLAddVariableValueAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isReplaceAll = u.isReplaceAll,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLAnyReceiveEvent
  (u: Uml#AnyReceiveEvent)
  : OTIMOFElement.OTIUMLAnyReceiveEvent
  = toOTIUMLAnyReceiveEvent(u, Option.empty[Document[Uml]])

  def toOTIUMLAnyReceiveEvent
  (u: UMLAnyReceiveEvent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLAnyReceiveEvent
  = OTIMOFElement.OTIUMLAnyReceiveEvent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLArtifact
  (u: Uml#Artifact)
  : OTIMOFElement.OTIUMLArtifact
  = toOTIUMLArtifact(u, Option.empty[Document[Uml]])

  def toOTIUMLArtifact
  (u: UMLArtifact[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLArtifact
  = OTIMOFElement.OTIUMLArtifact(
     otiMOFElementLocation = getElementLocationOf(u, context),
     fileName = u.fileName,
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLAssociation
  (u: Uml#Association)
  : OTIMOFElement.OTIUMLAssociation
  = toOTIUMLAssociation(u, Option.empty[Document[Uml]])

  def toOTIUMLAssociation
  (u: UMLAssociation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLAssociation
  = OTIMOFElement.OTIUMLAssociation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isDerived = u.isDerived,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLAssociationClass
  (u: Uml#AssociationClass)
  : OTIMOFElement.OTIUMLAssociationClass
  = toOTIUMLAssociationClass(u, Option.empty[Document[Uml]])

  def toOTIUMLAssociationClass
  (u: UMLAssociationClass[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLAssociationClass
  = OTIMOFElement.OTIUMLAssociationClass(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isDerived = u.isDerived,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLBehaviorExecutionSpecification
  (u: Uml#BehaviorExecutionSpecification)
  : OTIMOFElement.OTIUMLBehaviorExecutionSpecification
  = toOTIUMLBehaviorExecutionSpecification(u, Option.empty[Document[Uml]])

  def toOTIUMLBehaviorExecutionSpecification
  (u: UMLBehaviorExecutionSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLBehaviorExecutionSpecification
  = OTIMOFElement.OTIUMLBehaviorExecutionSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLBroadcastSignalAction
  (u: Uml#BroadcastSignalAction)
  : OTIMOFElement.OTIUMLBroadcastSignalAction
  = toOTIUMLBroadcastSignalAction(u, Option.empty[Document[Uml]])

  def toOTIUMLBroadcastSignalAction
  (u: UMLBroadcastSignalAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLBroadcastSignalAction
  = OTIMOFElement.OTIUMLBroadcastSignalAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLCallBehaviorAction
  (u: Uml#CallBehaviorAction)
  : OTIMOFElement.OTIUMLCallBehaviorAction
  = toOTIUMLCallBehaviorAction(u, Option.empty[Document[Uml]])

  def toOTIUMLCallBehaviorAction
  (u: UMLCallBehaviorAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLCallBehaviorAction
  = OTIMOFElement.OTIUMLCallBehaviorAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isSynchronous = u.isSynchronous,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLCallEvent
  (u: Uml#CallEvent)
  : OTIMOFElement.OTIUMLCallEvent
  = toOTIUMLCallEvent(u, Option.empty[Document[Uml]])

  def toOTIUMLCallEvent
  (u: UMLCallEvent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLCallEvent
  = OTIMOFElement.OTIUMLCallEvent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLCallOperationAction
  (u: Uml#CallOperationAction)
  : OTIMOFElement.OTIUMLCallOperationAction
  = toOTIUMLCallOperationAction(u, Option.empty[Document[Uml]])

  def toOTIUMLCallOperationAction
  (u: UMLCallOperationAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLCallOperationAction
  = OTIMOFElement.OTIUMLCallOperationAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isSynchronous = u.isSynchronous,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLCentralBufferNode
  (u: Uml#CentralBufferNode)
  : OTIMOFElement.OTIUMLCentralBufferNode
  = toOTIUMLCentralBufferNode(u, Option.empty[Document[Uml]])

  def toOTIUMLCentralBufferNode
  (u: UMLCentralBufferNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLCentralBufferNode
  = OTIMOFElement.OTIUMLCentralBufferNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)


  implicit def toOTIUMLChangeEvent
  (u: Uml#ChangeEvent)
  : OTIMOFElement.OTIUMLChangeEvent
  = toOTIUMLChangeEvent(u, Option.empty[Document[Uml]])

  def toOTIUMLChangeEvent
  (u: UMLChangeEvent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLChangeEvent
  = OTIMOFElement.OTIUMLChangeEvent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLClass
  (u: Uml#Class)
  : OTIMOFElement.OTIUMLClass
  = toOTIUMLClass(u, Option.empty[Document[Uml]])

  def toOTIUMLClass
  (u: UMLClass[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLClass
  = OTIMOFElement.OTIUMLClass(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLClassifierTemplateParameter
  (u: Uml#ClassifierTemplateParameter)
  : OTIMOFElement.OTIUMLClassifierTemplateParameter
  = toOTIUMLClassifierTemplateParameter(u, Option.empty[Document[Uml]])

  def toOTIUMLClassifierTemplateParameter
  (u: UMLClassifierTemplateParameter[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLClassifierTemplateParameter
  = OTIMOFElement.OTIUMLClassifierTemplateParameter(
     otiMOFElementLocation = getElementLocationOf(u, context),
     allowSubstitutable = u.allowSubstitutable)


  implicit def toOTIUMLClause
  (u: Uml#Clause)
  : OTIMOFElement.OTIUMLClause
  = toOTIUMLClause(u, Option.empty[Document[Uml]])

  def toOTIUMLClause
  (u: UMLClause[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLClause
  = OTIMOFElement.OTIUMLClause(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLClearAssociationAction
  (u: Uml#ClearAssociationAction)
  : OTIMOFElement.OTIUMLClearAssociationAction
  = toOTIUMLClearAssociationAction(u, Option.empty[Document[Uml]])

  def toOTIUMLClearAssociationAction
  (u: UMLClearAssociationAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLClearAssociationAction
  = OTIMOFElement.OTIUMLClearAssociationAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLClearStructuralFeatureAction
  (u: Uml#ClearStructuralFeatureAction)
  : OTIMOFElement.OTIUMLClearStructuralFeatureAction
  = toOTIUMLClearStructuralFeatureAction(u, Option.empty[Document[Uml]])

  def toOTIUMLClearStructuralFeatureAction
  (u: UMLClearStructuralFeatureAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLClearStructuralFeatureAction
  = OTIMOFElement.OTIUMLClearStructuralFeatureAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLClearVariableAction
  (u: Uml#ClearVariableAction)
  : OTIMOFElement.OTIUMLClearVariableAction
  = toOTIUMLClearVariableAction(u, Option.empty[Document[Uml]])

  def toOTIUMLClearVariableAction
  (u: UMLClearVariableAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLClearVariableAction
  = OTIMOFElement.OTIUMLClearVariableAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLCollaboration
  (u: Uml#Collaboration)
  : OTIMOFElement.OTIUMLCollaboration
  = toOTIUMLCollaboration(u, Option.empty[Document[Uml]])

  def toOTIUMLCollaboration
  (u: UMLCollaboration[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLCollaboration
  = OTIMOFElement.OTIUMLCollaboration(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLCollaborationUse
  (u: Uml#CollaborationUse)
  : OTIMOFElement.OTIUMLCollaborationUse
  = toOTIUMLCollaborationUse(u, Option.empty[Document[Uml]])

  def toOTIUMLCollaborationUse
  (u: UMLCollaborationUse[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLCollaborationUse
  = OTIMOFElement.OTIUMLCollaborationUse(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLCombinedFragment
  (u: Uml#CombinedFragment)
  : OTIMOFElement.OTIUMLCombinedFragment
  = toOTIUMLCombinedFragment(u, Option.empty[Document[Uml]])

  def toOTIUMLCombinedFragment
  (u: UMLCombinedFragment[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLCombinedFragment
  = OTIMOFElement.OTIUMLCombinedFragment(
     otiMOFElementLocation = getElementLocationOf(u, context),
     interactionOperator = u.interactionOperator,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLComment
  (u: Uml#Comment)
  : OTIMOFElement.OTIUMLComment
  = toOTIUMLComment(u, Option.empty[Document[Uml]])

  def toOTIUMLComment
  (u: UMLComment[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLComment
  = OTIMOFElement.OTIUMLComment(
     otiMOFElementLocation = getElementLocationOf(u, context),
     body = u.body)


  implicit def toOTIUMLCommunicationPath
  (u: Uml#CommunicationPath)
  : OTIMOFElement.OTIUMLCommunicationPath
  = toOTIUMLCommunicationPath(u, Option.empty[Document[Uml]])

  def toOTIUMLCommunicationPath
  (u: UMLCommunicationPath[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLCommunicationPath
  = OTIMOFElement.OTIUMLCommunicationPath(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isDerived = u.isDerived,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLComponent
  (u: Uml#Component)
  : OTIMOFElement.OTIUMLComponent
  = toOTIUMLComponent(u, Option.empty[Document[Uml]])

  def toOTIUMLComponent
  (u: UMLComponent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLComponent
  = OTIMOFElement.OTIUMLComponent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isIndirectlyInstantiated = u.isIndirectlyInstantiated,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLComponentRealization
  (u: Uml#ComponentRealization)
  : OTIMOFElement.OTIUMLComponentRealization
  = toOTIUMLComponentRealization(u, Option.empty[Document[Uml]])

  def toOTIUMLComponentRealization
  (u: UMLComponentRealization[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLComponentRealization
  = OTIMOFElement.OTIUMLComponentRealization(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLConditionalNode
  (u: Uml#ConditionalNode)
  : OTIMOFElement.OTIUMLConditionalNode
  = toOTIUMLConditionalNode(u, Option.empty[Document[Uml]])

  def toOTIUMLConditionalNode
  (u: UMLConditionalNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLConditionalNode
  = OTIMOFElement.OTIUMLConditionalNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAssured = u.isAssured,
     isDeterminate = u.isDeterminate,
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     mustIsolate = u.mustIsolate,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLConnectableElementTemplateParameter
  (u: Uml#ConnectableElementTemplateParameter)
  : OTIMOFElement.OTIUMLConnectableElementTemplateParameter
  = toOTIUMLConnectableElementTemplateParameter(u, Option.empty[Document[Uml]])

  def toOTIUMLConnectableElementTemplateParameter
  (u: UMLConnectableElementTemplateParameter[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLConnectableElementTemplateParameter
  = OTIMOFElement.OTIUMLConnectableElementTemplateParameter(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLConnectionPointReference
  (u: Uml#ConnectionPointReference)
  : OTIMOFElement.OTIUMLConnectionPointReference
  = toOTIUMLConnectionPointReference(u, Option.empty[Document[Uml]])

  def toOTIUMLConnectionPointReference
  (u: UMLConnectionPointReference[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLConnectionPointReference
  = OTIMOFElement.OTIUMLConnectionPointReference(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLConnector
  (u: Uml#Connector)
  : OTIMOFElement.OTIUMLConnector
  = toOTIUMLConnector(u, Option.empty[Document[Uml]])

  def toOTIUMLConnector
  (u: UMLConnector[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLConnector
  = OTIMOFElement.OTIUMLConnector(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isStatic = u.isStatic,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLConnectorEnd
  (u: Uml#ConnectorEnd)
  : OTIMOFElement.OTIUMLConnectorEnd
  = toOTIUMLConnectorEnd(u, Option.empty[Document[Uml]])

  def toOTIUMLConnectorEnd
  (u: UMLConnectorEnd[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLConnectorEnd
  = OTIMOFElement.OTIUMLConnectorEnd(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isOrdered = u.isOrdered,
     isUnique = u.isUnique)


  implicit def toOTIUMLConsiderIgnoreFragment
  (u: Uml#ConsiderIgnoreFragment)
  : OTIMOFElement.OTIUMLConsiderIgnoreFragment
  = toOTIUMLConsiderIgnoreFragment(u, Option.empty[Document[Uml]])

  def toOTIUMLConsiderIgnoreFragment
  (u: UMLConsiderIgnoreFragment[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLConsiderIgnoreFragment
  = OTIMOFElement.OTIUMLConsiderIgnoreFragment(
     otiMOFElementLocation = getElementLocationOf(u, context),
     interactionOperator = u.interactionOperator,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLConstraint
  (u: Uml#Constraint)
  : OTIMOFElement.OTIUMLConstraint
  = toOTIUMLConstraint(u, Option.empty[Document[Uml]])

  def toOTIUMLConstraint
  (u: UMLConstraint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLConstraint
  = OTIMOFElement.OTIUMLConstraint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLContinuation
  (u: Uml#Continuation)
  : OTIMOFElement.OTIUMLContinuation
  = toOTIUMLContinuation(u, Option.empty[Document[Uml]])

  def toOTIUMLContinuation
  (u: UMLContinuation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLContinuation
  = OTIMOFElement.OTIUMLContinuation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     setting = u.setting,
     visibility = u.visibility)


  implicit def toOTIUMLControlFlow
  (u: Uml#ControlFlow)
  : OTIMOFElement.OTIUMLControlFlow
  = toOTIUMLControlFlow(u, Option.empty[Document[Uml]])

  def toOTIUMLControlFlow
  (u: UMLControlFlow[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLControlFlow
  = OTIMOFElement.OTIUMLControlFlow(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLCreateLinkAction
  (u: Uml#CreateLinkAction)
  : OTIMOFElement.OTIUMLCreateLinkAction
  = toOTIUMLCreateLinkAction(u, Option.empty[Document[Uml]])

  def toOTIUMLCreateLinkAction
  (u: UMLCreateLinkAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLCreateLinkAction
  = OTIMOFElement.OTIUMLCreateLinkAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLCreateLinkObjectAction
  (u: Uml#CreateLinkObjectAction)
  : OTIMOFElement.OTIUMLCreateLinkObjectAction
  = toOTIUMLCreateLinkObjectAction(u, Option.empty[Document[Uml]])

  def toOTIUMLCreateLinkObjectAction
  (u: UMLCreateLinkObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLCreateLinkObjectAction
  = OTIMOFElement.OTIUMLCreateLinkObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLCreateObjectAction
  (u: Uml#CreateObjectAction)
  : OTIMOFElement.OTIUMLCreateObjectAction
  = toOTIUMLCreateObjectAction(u, Option.empty[Document[Uml]])

  def toOTIUMLCreateObjectAction
  (u: UMLCreateObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLCreateObjectAction
  = OTIMOFElement.OTIUMLCreateObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDataStoreNode
  (u: Uml#DataStoreNode)
  : OTIMOFElement.OTIUMLDataStoreNode
  = toOTIUMLDataStoreNode(u, Option.empty[Document[Uml]])

  def toOTIUMLDataStoreNode
  (u: UMLDataStoreNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDataStoreNode
  = OTIMOFElement.OTIUMLDataStoreNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)


  implicit def toOTIUMLDataType
  (u: Uml#DataType)
  : OTIMOFElement.OTIUMLDataType
  = toOTIUMLDataType(u, Option.empty[Document[Uml]])

  def toOTIUMLDataType
  (u: UMLDataType[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDataType
  = OTIMOFElement.OTIUMLDataType(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDecisionNode
  (u: Uml#DecisionNode)
  : OTIMOFElement.OTIUMLDecisionNode
  = toOTIUMLDecisionNode(u, Option.empty[Document[Uml]])

  def toOTIUMLDecisionNode
  (u: UMLDecisionNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDecisionNode
  = OTIMOFElement.OTIUMLDecisionNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDependency
  (u: Uml#Dependency)
  : OTIMOFElement.OTIUMLDependency
  = toOTIUMLDependency(u, Option.empty[Document[Uml]])

  def toOTIUMLDependency
  (u: UMLDependency[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDependency
  = OTIMOFElement.OTIUMLDependency(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDeployment
  (u: Uml#Deployment)
  : OTIMOFElement.OTIUMLDeployment
  = toOTIUMLDeployment(u, Option.empty[Document[Uml]])

  def toOTIUMLDeployment
  (u: UMLDeployment[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDeployment
  = OTIMOFElement.OTIUMLDeployment(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDeploymentSpecification
  (u: Uml#DeploymentSpecification)
  : OTIMOFElement.OTIUMLDeploymentSpecification
  = toOTIUMLDeploymentSpecification(u, Option.empty[Document[Uml]])

  def toOTIUMLDeploymentSpecification
  (u: UMLDeploymentSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDeploymentSpecification
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


  implicit def toOTIUMLDestroyLinkAction
  (u: Uml#DestroyLinkAction)
  : OTIMOFElement.OTIUMLDestroyLinkAction
  = toOTIUMLDestroyLinkAction(u, Option.empty[Document[Uml]])

  def toOTIUMLDestroyLinkAction
  (u: UMLDestroyLinkAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDestroyLinkAction
  = OTIMOFElement.OTIUMLDestroyLinkAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDestroyObjectAction
  (u: Uml#DestroyObjectAction)
  : OTIMOFElement.OTIUMLDestroyObjectAction
  = toOTIUMLDestroyObjectAction(u, Option.empty[Document[Uml]])

  def toOTIUMLDestroyObjectAction
  (u: UMLDestroyObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDestroyObjectAction
  = OTIMOFElement.OTIUMLDestroyObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isDestroyLinks = u.isDestroyLinks,
     isDestroyOwnedObjects = u.isDestroyOwnedObjects,
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDestructionOccurrenceSpecification
  (u: Uml#DestructionOccurrenceSpecification)
  : OTIMOFElement.OTIUMLDestructionOccurrenceSpecification
  = toOTIUMLDestructionOccurrenceSpecification(u, Option.empty[Document[Uml]])

  def toOTIUMLDestructionOccurrenceSpecification
  (u: UMLDestructionOccurrenceSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDestructionOccurrenceSpecification
  = OTIMOFElement.OTIUMLDestructionOccurrenceSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDevice
  (u: Uml#Device)
  : OTIMOFElement.OTIUMLDevice
  = toOTIUMLDevice(u, Option.empty[Document[Uml]])

  def toOTIUMLDevice
  (u: UMLDevice[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDevice
  = OTIMOFElement.OTIUMLDevice(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDiagram
  (u: Uml#Diagram)
  : OTIMOFElement.OTIUMLDiagram
  = toOTIUMLDiagram(u, Option.empty[Document[Uml]])

  def toOTIUMLDiagram
  (u: UMLDiagram[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDiagram
  = OTIMOFElement.OTIUMLDiagram(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDuration
  (u: Uml#Duration)
  : OTIMOFElement.OTIUMLDuration
  = toOTIUMLDuration(u, Option.empty[Document[Uml]])

  def toOTIUMLDuration
  (u: UMLDuration[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDuration
  = OTIMOFElement.OTIUMLDuration(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDurationConstraint
  (u: Uml#DurationConstraint)
  : OTIMOFElement.OTIUMLDurationConstraint
  = toOTIUMLDurationConstraint(u, Option.empty[Document[Uml]])

  def toOTIUMLDurationConstraint
  (u: UMLDurationConstraint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDurationConstraint
  = OTIMOFElement.OTIUMLDurationConstraint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     firstEvent = u.firstEvent,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDurationInterval
  (u: Uml#DurationInterval)
  : OTIMOFElement.OTIUMLDurationInterval
  = toOTIUMLDurationInterval(u, Option.empty[Document[Uml]])

  def toOTIUMLDurationInterval
  (u: UMLDurationInterval[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDurationInterval
  = OTIMOFElement.OTIUMLDurationInterval(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLDurationObservation
  (u: Uml#DurationObservation)
  : OTIMOFElement.OTIUMLDurationObservation
  = toOTIUMLDurationObservation(u, Option.empty[Document[Uml]])

  def toOTIUMLDurationObservation
  (u: UMLDurationObservation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLDurationObservation
  = OTIMOFElement.OTIUMLDurationObservation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     firstEvent = u.firstEvent,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLElementImport
  (u: Uml#ElementImport)
  : OTIMOFElement.OTIUMLElementImport
  = toOTIUMLElementImport(u, Option.empty[Document[Uml]])

  def toOTIUMLElementImport
  (u: UMLElementImport[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLElementImport
  = OTIMOFElement.OTIUMLElementImport(
     otiMOFElementLocation = getElementLocationOf(u, context),
     alias = u.alias,
     visibility = u.visibility)


  implicit def toOTIUMLElementValue
  (u: Uml#ElementValue)
  : OTIMOFElement.OTIUMLElementValue
  = toOTIUMLElementValue(u, Option.empty[Document[Uml]])

  def toOTIUMLElementValue
  (u: UMLElementValue[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLElementValue
  = OTIMOFElement.OTIUMLElementValue(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLEnumeration
  (u: Uml#Enumeration)
  : OTIMOFElement.OTIUMLEnumeration
  = toOTIUMLEnumeration(u, Option.empty[Document[Uml]])

  def toOTIUMLEnumeration
  (u: UMLEnumeration[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLEnumeration
  = OTIMOFElement.OTIUMLEnumeration(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLEnumerationLiteral
  (u: Uml#EnumerationLiteral)
  : OTIMOFElement.OTIUMLEnumerationLiteral
  = toOTIUMLEnumerationLiteral(u, Option.empty[Document[Uml]])

  def toOTIUMLEnumerationLiteral
  (u: UMLEnumerationLiteral[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLEnumerationLiteral
  = OTIMOFElement.OTIUMLEnumerationLiteral(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLExceptionHandler
  (u: Uml#ExceptionHandler)
  : OTIMOFElement.OTIUMLExceptionHandler
  = toOTIUMLExceptionHandler(u, Option.empty[Document[Uml]])

  def toOTIUMLExceptionHandler
  (u: UMLExceptionHandler[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLExceptionHandler
  = OTIMOFElement.OTIUMLExceptionHandler(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLExecutionEnvironment
  (u: Uml#ExecutionEnvironment)
  : OTIMOFElement.OTIUMLExecutionEnvironment
  = toOTIUMLExecutionEnvironment(u, Option.empty[Document[Uml]])

  def toOTIUMLExecutionEnvironment
  (u: UMLExecutionEnvironment[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLExecutionEnvironment
  = OTIMOFElement.OTIUMLExecutionEnvironment(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLExecutionOccurrenceSpecification
  (u: Uml#ExecutionOccurrenceSpecification)
  : OTIMOFElement.OTIUMLExecutionOccurrenceSpecification
  = toOTIUMLExecutionOccurrenceSpecification(u, Option.empty[Document[Uml]])

  def toOTIUMLExecutionOccurrenceSpecification
  (u: UMLExecutionOccurrenceSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLExecutionOccurrenceSpecification
  = OTIMOFElement.OTIUMLExecutionOccurrenceSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLExpansionNode
  (u: Uml#ExpansionNode)
  : OTIMOFElement.OTIUMLExpansionNode
  = toOTIUMLExpansionNode(u, Option.empty[Document[Uml]])

  def toOTIUMLExpansionNode
  (u: UMLExpansionNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLExpansionNode
  = OTIMOFElement.OTIUMLExpansionNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isControlType = u.isControlType,
     isLeaf = u.isLeaf,
     name = u.name,
     ordering = u.ordering,
     visibility = u.visibility)


  implicit def toOTIUMLExpansionRegion
  (u: Uml#ExpansionRegion)
  : OTIMOFElement.OTIUMLExpansionRegion
  = toOTIUMLExpansionRegion(u, Option.empty[Document[Uml]])

  def toOTIUMLExpansionRegion
  (u: UMLExpansionRegion[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLExpansionRegion
  = OTIMOFElement.OTIUMLExpansionRegion(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     mode = u.mode,
     mustIsolate = u.mustIsolate,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLExpression
  (u: Uml#Expression)
  : OTIMOFElement.OTIUMLExpression
  = toOTIUMLExpression(u, Option.empty[Document[Uml]])

  def toOTIUMLExpression
  (u: UMLExpression[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLExpression
  = OTIMOFElement.OTIUMLExpression(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     symbol = u.symbol,
     visibility = u.visibility)


  implicit def toOTIUMLExtend
  (u: Uml#Extend)
  : OTIMOFElement.OTIUMLExtend
  = toOTIUMLExtend(u, Option.empty[Document[Uml]])

  def toOTIUMLExtend
  (u: UMLExtend[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLExtend
  = OTIMOFElement.OTIUMLExtend(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLExtension
  (u: Uml#Extension)
  : OTIMOFElement.OTIUMLExtension
  = toOTIUMLExtension(u, Option.empty[Document[Uml]])

  def toOTIUMLExtension
  (u: UMLExtension[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLExtension
  = OTIMOFElement.OTIUMLExtension(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isDerived = u.isDerived,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLExtensionEnd
  (u: Uml#ExtensionEnd)
  : OTIMOFElement.OTIUMLExtensionEnd
  = toOTIUMLExtensionEnd(u, Option.empty[Document[Uml]])

  def toOTIUMLExtensionEnd
  (u: UMLExtensionEnd[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLExtensionEnd
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


  implicit def toOTIUMLExtensionPoint
  (u: Uml#ExtensionPoint)
  : OTIMOFElement.OTIUMLExtensionPoint
  = toOTIUMLExtensionPoint(u, Option.empty[Document[Uml]])

  def toOTIUMLExtensionPoint
  (u: UMLExtensionPoint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLExtensionPoint
  = OTIMOFElement.OTIUMLExtensionPoint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLFinalState
  (u: Uml#FinalState)
  : OTIMOFElement.OTIUMLFinalState
  = toOTIUMLFinalState(u, Option.empty[Document[Uml]])

  def toOTIUMLFinalState
  (u: UMLFinalState[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLFinalState
  = OTIMOFElement.OTIUMLFinalState(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLFlowFinalNode
  (u: Uml#FlowFinalNode)
  : OTIMOFElement.OTIUMLFlowFinalNode
  = toOTIUMLFlowFinalNode(u, Option.empty[Document[Uml]])

  def toOTIUMLFlowFinalNode
  (u: UMLFlowFinalNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLFlowFinalNode
  = OTIMOFElement.OTIUMLFlowFinalNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLForkNode
  (u: Uml#ForkNode)
  : OTIMOFElement.OTIUMLForkNode
  = toOTIUMLForkNode(u, Option.empty[Document[Uml]])

  def toOTIUMLForkNode
  (u: UMLForkNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLForkNode
  = OTIMOFElement.OTIUMLForkNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLFunctionBehavior
  (u: Uml#FunctionBehavior)
  : OTIMOFElement.OTIUMLFunctionBehavior
  = toOTIUMLFunctionBehavior(u, Option.empty[Document[Uml]])

  def toOTIUMLFunctionBehavior
  (u: UMLFunctionBehavior[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLFunctionBehavior
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


  implicit def toOTIUMLGate
  (u: Uml#Gate)
  : OTIMOFElement.OTIUMLGate
  = toOTIUMLGate(u, Option.empty[Document[Uml]])

  def toOTIUMLGate
  (u: UMLGate[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLGate
  = OTIMOFElement.OTIUMLGate(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLGeneralOrdering
  (u: Uml#GeneralOrdering)
  : OTIMOFElement.OTIUMLGeneralOrdering
  = toOTIUMLGeneralOrdering(u, Option.empty[Document[Uml]])

  def toOTIUMLGeneralOrdering
  (u: UMLGeneralOrdering[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLGeneralOrdering
  = OTIMOFElement.OTIUMLGeneralOrdering(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLGeneralization
  (u: Uml#Generalization)
  : OTIMOFElement.OTIUMLGeneralization
  = toOTIUMLGeneralization(u, Option.empty[Document[Uml]])

  def toOTIUMLGeneralization
  (u: UMLGeneralization[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLGeneralization
  = OTIMOFElement.OTIUMLGeneralization(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isSubstitutable = u.isSubstitutable)


  implicit def toOTIUMLGeneralizationSet
  (u: Uml#GeneralizationSet)
  : OTIMOFElement.OTIUMLGeneralizationSet
  = toOTIUMLGeneralizationSet(u, Option.empty[Document[Uml]])

  def toOTIUMLGeneralizationSet
  (u: UMLGeneralizationSet[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLGeneralizationSet
  = OTIMOFElement.OTIUMLGeneralizationSet(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isCovering = u.isCovering,
     isDisjoint = u.isDisjoint,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLImage
  (u: Uml#Image)
  : OTIMOFElement.OTIUMLImage
  = toOTIUMLImage(u, Option.empty[Document[Uml]])

  def toOTIUMLImage
  (u: UMLImage[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLImage
  = OTIMOFElement.OTIUMLImage(
     otiMOFElementLocation = getElementLocationOf(u, context),
     content = u.content,
     format = u.format,
     location = u.location)


  implicit def toOTIUMLInclude
  (u: Uml#Include)
  : OTIMOFElement.OTIUMLInclude
  = toOTIUMLInclude(u, Option.empty[Document[Uml]])

  def toOTIUMLInclude
  (u: UMLInclude[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInclude
  = OTIMOFElement.OTIUMLInclude(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInformationFlow
  (u: Uml#InformationFlow)
  : OTIMOFElement.OTIUMLInformationFlow
  = toOTIUMLInformationFlow(u, Option.empty[Document[Uml]])

  def toOTIUMLInformationFlow
  (u: UMLInformationFlow[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInformationFlow
  = OTIMOFElement.OTIUMLInformationFlow(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInformationItem
  (u: Uml#InformationItem)
  : OTIMOFElement.OTIUMLInformationItem
  = toOTIUMLInformationItem(u, Option.empty[Document[Uml]])

  def toOTIUMLInformationItem
  (u: UMLInformationItem[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInformationItem
  = OTIMOFElement.OTIUMLInformationItem(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInitialNode
  (u: Uml#InitialNode)
  : OTIMOFElement.OTIUMLInitialNode
  = toOTIUMLInitialNode(u, Option.empty[Document[Uml]])

  def toOTIUMLInitialNode
  (u: UMLInitialNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInitialNode
  = OTIMOFElement.OTIUMLInitialNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInputPin
  (u: Uml#InputPin)
  : OTIMOFElement.OTIUMLInputPin
  = toOTIUMLInputPin(u, Option.empty[Document[Uml]])

  def toOTIUMLInputPin
  (u: UMLInputPin[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInputPin
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


  implicit def toOTIUMLInstanceSpecification
  (u: Uml#InstanceSpecification)
  : OTIMOFElement.OTIUMLInstanceSpecification
  = toOTIUMLInstanceSpecification(u, Option.empty[Document[Uml]])

  def toOTIUMLInstanceSpecification
  (u: UMLInstanceSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInstanceSpecification
  = OTIMOFElement.OTIUMLInstanceSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInstanceValue
  (u: Uml#InstanceValue)
  : OTIMOFElement.OTIUMLInstanceValue
  = toOTIUMLInstanceValue(u, Option.empty[Document[Uml]])

  def toOTIUMLInstanceValue
  (u: UMLInstanceValue[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInstanceValue
  = OTIMOFElement.OTIUMLInstanceValue(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInteraction
  (u: Uml#Interaction)
  : OTIMOFElement.OTIUMLInteraction
  = toOTIUMLInteraction(u, Option.empty[Document[Uml]])

  def toOTIUMLInteraction
  (u: UMLInteraction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInteraction
  = OTIMOFElement.OTIUMLInteraction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     isReentrant = u.isReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInteractionConstraint
  (u: Uml#InteractionConstraint)
  : OTIMOFElement.OTIUMLInteractionConstraint
  = toOTIUMLInteractionConstraint(u, Option.empty[Document[Uml]])

  def toOTIUMLInteractionConstraint
  (u: UMLInteractionConstraint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInteractionConstraint
  = OTIMOFElement.OTIUMLInteractionConstraint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInteractionOperand
  (u: Uml#InteractionOperand)
  : OTIMOFElement.OTIUMLInteractionOperand
  = toOTIUMLInteractionOperand(u, Option.empty[Document[Uml]])

  def toOTIUMLInteractionOperand
  (u: UMLInteractionOperand[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInteractionOperand
  = OTIMOFElement.OTIUMLInteractionOperand(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInteractionUse
  (u: Uml#InteractionUse)
  : OTIMOFElement.OTIUMLInteractionUse
  = toOTIUMLInteractionUse(u, Option.empty[Document[Uml]])

  def toOTIUMLInteractionUse
  (u: UMLInteractionUse[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInteractionUse
  = OTIMOFElement.OTIUMLInteractionUse(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInterface
  (u: Uml#Interface)
  : OTIMOFElement.OTIUMLInterface
  = toOTIUMLInterface(u, Option.empty[Document[Uml]])

  def toOTIUMLInterface
  (u: UMLInterface[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInterface
  = OTIMOFElement.OTIUMLInterface(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInterfaceRealization
  (u: Uml#InterfaceRealization)
  : OTIMOFElement.OTIUMLInterfaceRealization
  = toOTIUMLInterfaceRealization(u, Option.empty[Document[Uml]])

  def toOTIUMLInterfaceRealization
  (u: UMLInterfaceRealization[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInterfaceRealization
  = OTIMOFElement.OTIUMLInterfaceRealization(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInterruptibleActivityRegion
  (u: Uml#InterruptibleActivityRegion)
  : OTIMOFElement.OTIUMLInterruptibleActivityRegion
  = toOTIUMLInterruptibleActivityRegion(u, Option.empty[Document[Uml]])

  def toOTIUMLInterruptibleActivityRegion
  (u: UMLInterruptibleActivityRegion[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInterruptibleActivityRegion
  = OTIMOFElement.OTIUMLInterruptibleActivityRegion(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLInterval
  (u: Uml#Interval)
  : OTIMOFElement.OTIUMLInterval
  = toOTIUMLInterval(u, Option.empty[Document[Uml]])

  def toOTIUMLInterval
  (u: UMLInterval[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLInterval
  = OTIMOFElement.OTIUMLInterval(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLIntervalConstraint
  (u: Uml#IntervalConstraint)
  : OTIMOFElement.OTIUMLIntervalConstraint
  = toOTIUMLIntervalConstraint(u, Option.empty[Document[Uml]])

  def toOTIUMLIntervalConstraint
  (u: UMLIntervalConstraint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLIntervalConstraint
  = OTIMOFElement.OTIUMLIntervalConstraint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLJoinNode
  (u: Uml#JoinNode)
  : OTIMOFElement.OTIUMLJoinNode
  = toOTIUMLJoinNode(u, Option.empty[Document[Uml]])

  def toOTIUMLJoinNode
  (u: UMLJoinNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLJoinNode
  = OTIMOFElement.OTIUMLJoinNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isCombineDuplicate = u.isCombineDuplicate,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLLifeline
  (u: Uml#Lifeline)
  : OTIMOFElement.OTIUMLLifeline
  = toOTIUMLLifeline(u, Option.empty[Document[Uml]])

  def toOTIUMLLifeline
  (u: UMLLifeline[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLLifeline
  = OTIMOFElement.OTIUMLLifeline(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLLinkEndCreationData
  (u: Uml#LinkEndCreationData)
  : OTIMOFElement.OTIUMLLinkEndCreationData
  = toOTIUMLLinkEndCreationData(u, Option.empty[Document[Uml]])

  def toOTIUMLLinkEndCreationData
  (u: UMLLinkEndCreationData[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLLinkEndCreationData
  = OTIMOFElement.OTIUMLLinkEndCreationData(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isReplaceAll = u.isReplaceAll)


  implicit def toOTIUMLLinkEndData
  (u: Uml#LinkEndData)
  : OTIMOFElement.OTIUMLLinkEndData
  = toOTIUMLLinkEndData(u, Option.empty[Document[Uml]])

  def toOTIUMLLinkEndData
  (u: UMLLinkEndData[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLLinkEndData
  = OTIMOFElement.OTIUMLLinkEndData(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLLinkEndDestructionData
  (u: Uml#LinkEndDestructionData)
  : OTIMOFElement.OTIUMLLinkEndDestructionData
  = toOTIUMLLinkEndDestructionData(u, Option.empty[Document[Uml]])

  def toOTIUMLLinkEndDestructionData
  (u: UMLLinkEndDestructionData[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLLinkEndDestructionData
  = OTIMOFElement.OTIUMLLinkEndDestructionData(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isDestroyDuplicates = u.isDestroyDuplicates)


  implicit def toOTIUMLLiteralBoolean
  (u: Uml#LiteralBoolean)
  : OTIMOFElement.OTIUMLLiteralBoolean
  = toOTIUMLLiteralBoolean(u, Option.empty[Document[Uml]])

  def toOTIUMLLiteralBoolean
  (u: UMLLiteralBoolean[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLLiteralBoolean
  = OTIMOFElement.OTIUMLLiteralBoolean(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     value = u.value,
     visibility = u.visibility)


  implicit def toOTIUMLLiteralInteger
  (u: Uml#LiteralInteger)
  : OTIMOFElement.OTIUMLLiteralInteger
  = toOTIUMLLiteralInteger(u, Option.empty[Document[Uml]])

  def toOTIUMLLiteralInteger
  (u: UMLLiteralInteger[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLLiteralInteger
  = OTIMOFElement.OTIUMLLiteralInteger(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     value = u.value,
     visibility = u.visibility)


  implicit def toOTIUMLLiteralNull
  (u: Uml#LiteralNull)
  : OTIMOFElement.OTIUMLLiteralNull
  = toOTIUMLLiteralNull(u, Option.empty[Document[Uml]])

  def toOTIUMLLiteralNull
  (u: UMLLiteralNull[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLLiteralNull
  = OTIMOFElement.OTIUMLLiteralNull(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLLiteralReal
  (u: Uml#LiteralReal)
  : OTIMOFElement.OTIUMLLiteralReal
  = toOTIUMLLiteralReal(u, Option.empty[Document[Uml]])

  def toOTIUMLLiteralReal
  (u: UMLLiteralReal[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLLiteralReal
  = OTIMOFElement.OTIUMLLiteralReal(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     value = u.value,
     visibility = u.visibility)


  implicit def toOTIUMLLiteralString
  (u: Uml#LiteralString)
  : OTIMOFElement.OTIUMLLiteralString
  = toOTIUMLLiteralString(u, Option.empty[Document[Uml]])

  def toOTIUMLLiteralString
  (u: UMLLiteralString[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLLiteralString
  = OTIMOFElement.OTIUMLLiteralString(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     value = u.value,
     visibility = u.visibility)


  implicit def toOTIUMLLiteralUnlimitedNatural
  (u: Uml#LiteralUnlimitedNatural)
  : OTIMOFElement.OTIUMLLiteralUnlimitedNatural
  = toOTIUMLLiteralUnlimitedNatural(u, Option.empty[Document[Uml]])

  def toOTIUMLLiteralUnlimitedNatural
  (u: UMLLiteralUnlimitedNatural[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLLiteralUnlimitedNatural
  = OTIMOFElement.OTIUMLLiteralUnlimitedNatural(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     value = u.value,
     visibility = u.visibility)


  implicit def toOTIUMLLoopNode
  (u: Uml#LoopNode)
  : OTIMOFElement.OTIUMLLoopNode
  = toOTIUMLLoopNode(u, Option.empty[Document[Uml]])

  def toOTIUMLLoopNode
  (u: UMLLoopNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLLoopNode
  = OTIMOFElement.OTIUMLLoopNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isTestedFirst = u.isTestedFirst,
     mustIsolate = u.mustIsolate,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLManifestation
  (u: Uml#Manifestation)
  : OTIMOFElement.OTIUMLManifestation
  = toOTIUMLManifestation(u, Option.empty[Document[Uml]])

  def toOTIUMLManifestation
  (u: UMLManifestation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLManifestation
  = OTIMOFElement.OTIUMLManifestation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLMergeNode
  (u: Uml#MergeNode)
  : OTIMOFElement.OTIUMLMergeNode
  = toOTIUMLMergeNode(u, Option.empty[Document[Uml]])

  def toOTIUMLMergeNode
  (u: UMLMergeNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLMergeNode
  = OTIMOFElement.OTIUMLMergeNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLMessage
  (u: Uml#Message)
  : OTIMOFElement.OTIUMLMessage
  = toOTIUMLMessage(u, Option.empty[Document[Uml]])

  def toOTIUMLMessage
  (u: UMLMessage[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLMessage
  = OTIMOFElement.OTIUMLMessage(
     otiMOFElementLocation = getElementLocationOf(u, context),
     messageSort = u.messageSort,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLMessageOccurrenceSpecification
  (u: Uml#MessageOccurrenceSpecification)
  : OTIMOFElement.OTIUMLMessageOccurrenceSpecification
  = toOTIUMLMessageOccurrenceSpecification(u, Option.empty[Document[Uml]])

  def toOTIUMLMessageOccurrenceSpecification
  (u: UMLMessageOccurrenceSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLMessageOccurrenceSpecification
  = OTIMOFElement.OTIUMLMessageOccurrenceSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLModel
  (u: Uml#Model)
  : OTIMOFElement.OTIUMLModel
  = toOTIUMLModel(u, Option.empty[Document[Uml]])

  def toOTIUMLModel
  (u: UMLModel[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLModel
  = OTIMOFElement.OTIUMLModel(
     otiMOFElementLocation = getElementLocationOf(u, context),
     URI = u.URI,
     name = u.name,
     viewpoint = u.viewpoint,
     visibility = u.visibility)


  implicit def toOTIUMLNode
  (u: Uml#Node)
  : OTIMOFElement.OTIUMLNode
  = toOTIUMLNode(u, Option.empty[Document[Uml]])

  def toOTIUMLNode
  (u: UMLNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLNode
  = OTIMOFElement.OTIUMLNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLObjectFlow
  (u: Uml#ObjectFlow)
  : OTIMOFElement.OTIUMLObjectFlow
  = toOTIUMLObjectFlow(u, Option.empty[Document[Uml]])

  def toOTIUMLObjectFlow
  (u: UMLObjectFlow[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLObjectFlow
  = OTIMOFElement.OTIUMLObjectFlow(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isMulticast = u.isMulticast,
     isMultireceive = u.isMultireceive,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLOccurrenceSpecification
  (u: Uml#OccurrenceSpecification)
  : OTIMOFElement.OTIUMLOccurrenceSpecification
  = toOTIUMLOccurrenceSpecification(u, Option.empty[Document[Uml]])

  def toOTIUMLOccurrenceSpecification
  (u: UMLOccurrenceSpecification[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLOccurrenceSpecification
  = OTIMOFElement.OTIUMLOccurrenceSpecification(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLOpaqueAction
  (u: Uml#OpaqueAction)
  : OTIMOFElement.OTIUMLOpaqueAction
  = toOTIUMLOpaqueAction(u, Option.empty[Document[Uml]])

  def toOTIUMLOpaqueAction
  (u: UMLOpaqueAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLOpaqueAction
  = OTIMOFElement.OTIUMLOpaqueAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     body = u.body,
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     language = u.language,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLOpaqueBehavior
  (u: Uml#OpaqueBehavior)
  : OTIMOFElement.OTIUMLOpaqueBehavior
  = toOTIUMLOpaqueBehavior(u, Option.empty[Document[Uml]])

  def toOTIUMLOpaqueBehavior
  (u: UMLOpaqueBehavior[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLOpaqueBehavior
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


  implicit def toOTIUMLOpaqueExpression
  (u: Uml#OpaqueExpression)
  : OTIMOFElement.OTIUMLOpaqueExpression
  = toOTIUMLOpaqueExpression(u, Option.empty[Document[Uml]])

  def toOTIUMLOpaqueExpression
  (u: UMLOpaqueExpression[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLOpaqueExpression
  = OTIMOFElement.OTIUMLOpaqueExpression(
     otiMOFElementLocation = getElementLocationOf(u, context),
     body = u.body,
     language = u.language,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLOperation
  (u: Uml#Operation)
  : OTIMOFElement.OTIUMLOperation
  = toOTIUMLOperation(u, Option.empty[Document[Uml]])

  def toOTIUMLOperation
  (u: UMLOperation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLOperation
  = OTIMOFElement.OTIUMLOperation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     concurrency = u.concurrency,
     isAbstract = u.isAbstract,
     isLeaf = u.isLeaf,
     isQuery = u.isQuery,
     isStatic = u.isStatic,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLOperationTemplateParameter
  (u: Uml#OperationTemplateParameter)
  : OTIMOFElement.OTIUMLOperationTemplateParameter
  = toOTIUMLOperationTemplateParameter(u, Option.empty[Document[Uml]])

  def toOTIUMLOperationTemplateParameter
  (u: UMLOperationTemplateParameter[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLOperationTemplateParameter
  = OTIMOFElement.OTIUMLOperationTemplateParameter(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLOutputPin
  (u: Uml#OutputPin)
  : OTIMOFElement.OTIUMLOutputPin
  = toOTIUMLOutputPin(u, Option.empty[Document[Uml]])

  def toOTIUMLOutputPin
  (u: UMLOutputPin[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLOutputPin
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


  implicit def toOTIUMLPackage
  (u: Uml#Package)
  : OTIMOFElement.OTIUMLPackage
  = toOTIUMLPackage(u, Option.empty[Document[Uml]])

  def toOTIUMLPackage
  (u: UMLPackage[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLPackage
  = OTIMOFElement.OTIUMLPackage(
     otiMOFElementLocation = getElementLocationOf(u, context),
     URI = u.URI,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLPackageImport
  (u: Uml#PackageImport)
  : OTIMOFElement.OTIUMLPackageImport
  = toOTIUMLPackageImport(u, Option.empty[Document[Uml]])

  def toOTIUMLPackageImport
  (u: UMLPackageImport[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLPackageImport
  = OTIMOFElement.OTIUMLPackageImport(
     otiMOFElementLocation = getElementLocationOf(u, context),
     visibility = u.visibility)


  implicit def toOTIUMLPackageMerge
  (u: Uml#PackageMerge)
  : OTIMOFElement.OTIUMLPackageMerge
  = toOTIUMLPackageMerge(u, Option.empty[Document[Uml]])

  def toOTIUMLPackageMerge
  (u: UMLPackageMerge[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLPackageMerge
  = OTIMOFElement.OTIUMLPackageMerge(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLParameter
  (u: Uml#Parameter)
  : OTIMOFElement.OTIUMLParameter
  = toOTIUMLParameter(u, Option.empty[Document[Uml]])

  def toOTIUMLParameter
  (u: UMLParameter[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLParameter
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


  implicit def toOTIUMLParameterSet
  (u: Uml#ParameterSet)
  : OTIMOFElement.OTIUMLParameterSet
  = toOTIUMLParameterSet(u, Option.empty[Document[Uml]])

  def toOTIUMLParameterSet
  (u: UMLParameterSet[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLParameterSet
  = OTIMOFElement.OTIUMLParameterSet(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLPartDecomposition
  (u: Uml#PartDecomposition)
  : OTIMOFElement.OTIUMLPartDecomposition
  = toOTIUMLPartDecomposition(u, Option.empty[Document[Uml]])

  def toOTIUMLPartDecomposition
  (u: UMLPartDecomposition[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLPartDecomposition
  = OTIMOFElement.OTIUMLPartDecomposition(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLPort
  (u: Uml#Port)
  : OTIMOFElement.OTIUMLPort
  = toOTIUMLPort(u, Option.empty[Document[Uml]])

  def toOTIUMLPort
  (u: UMLPort[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLPort
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


  implicit def toOTIUMLPrimitiveType
  (u: Uml#PrimitiveType)
  : OTIMOFElement.OTIUMLPrimitiveType
  = toOTIUMLPrimitiveType(u, Option.empty[Document[Uml]])

  def toOTIUMLPrimitiveType
  (u: UMLPrimitiveType[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLPrimitiveType
  = OTIMOFElement.OTIUMLPrimitiveType(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLProfile
  (u: Uml#Profile)
  : OTIMOFElement.OTIUMLProfile
  = toOTIUMLProfile(u, Option.empty[Document[Uml]])

  def toOTIUMLProfile
  (u: UMLProfile[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLProfile
  = OTIMOFElement.OTIUMLProfile(
     otiMOFElementLocation = getElementLocationOf(u, context),
     URI = u.URI,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLProfileApplication
  (u: Uml#ProfileApplication)
  : OTIMOFElement.OTIUMLProfileApplication
  = toOTIUMLProfileApplication(u, Option.empty[Document[Uml]])

  def toOTIUMLProfileApplication
  (u: UMLProfileApplication[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLProfileApplication
  = OTIMOFElement.OTIUMLProfileApplication(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isStrict = u.isStrict)


  implicit def toOTIUMLProperty
  (u: Uml#Property)
  : OTIMOFElement.OTIUMLProperty
  = toOTIUMLProperty(u, Option.empty[Document[Uml]])

  def toOTIUMLProperty
  (u: UMLProperty[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLProperty
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


  implicit def toOTIUMLProtocolConformance
  (u: Uml#ProtocolConformance)
  : OTIMOFElement.OTIUMLProtocolConformance
  = toOTIUMLProtocolConformance(u, Option.empty[Document[Uml]])

  def toOTIUMLProtocolConformance
  (u: UMLProtocolConformance[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLProtocolConformance
  = OTIMOFElement.OTIUMLProtocolConformance(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLProtocolStateMachine
  (u: Uml#ProtocolStateMachine)
  : OTIMOFElement.OTIUMLProtocolStateMachine
  = toOTIUMLProtocolStateMachine(u, Option.empty[Document[Uml]])

  def toOTIUMLProtocolStateMachine
  (u: UMLProtocolStateMachine[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLProtocolStateMachine
  = OTIMOFElement.OTIUMLProtocolStateMachine(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     isReentrant = u.isReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLProtocolTransition
  (u: Uml#ProtocolTransition)
  : OTIMOFElement.OTIUMLProtocolTransition
  = toOTIUMLProtocolTransition(u, Option.empty[Document[Uml]])

  def toOTIUMLProtocolTransition
  (u: UMLProtocolTransition[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLProtocolTransition
  = OTIMOFElement.OTIUMLProtocolTransition(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     kind = u.kind,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLPseudostate
  (u: Uml#Pseudostate)
  : OTIMOFElement.OTIUMLPseudostate
  = toOTIUMLPseudostate(u, Option.empty[Document[Uml]])

  def toOTIUMLPseudostate
  (u: UMLPseudostate[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLPseudostate
  = OTIMOFElement.OTIUMLPseudostate(
     otiMOFElementLocation = getElementLocationOf(u, context),
     kind = u.kind,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLQualifierValue
  (u: Uml#QualifierValue)
  : OTIMOFElement.OTIUMLQualifierValue
  = toOTIUMLQualifierValue(u, Option.empty[Document[Uml]])

  def toOTIUMLQualifierValue
  (u: UMLQualifierValue[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLQualifierValue
  = OTIMOFElement.OTIUMLQualifierValue(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLRaiseExceptionAction
  (u: Uml#RaiseExceptionAction)
  : OTIMOFElement.OTIUMLRaiseExceptionAction
  = toOTIUMLRaiseExceptionAction(u, Option.empty[Document[Uml]])

  def toOTIUMLRaiseExceptionAction
  (u: UMLRaiseExceptionAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLRaiseExceptionAction
  = OTIMOFElement.OTIUMLRaiseExceptionAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReadExtentAction
  (u: Uml#ReadExtentAction)
  : OTIMOFElement.OTIUMLReadExtentAction
  = toOTIUMLReadExtentAction(u, Option.empty[Document[Uml]])

  def toOTIUMLReadExtentAction
  (u: UMLReadExtentAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReadExtentAction
  = OTIMOFElement.OTIUMLReadExtentAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReadIsClassifiedObjectAction
  (u: Uml#ReadIsClassifiedObjectAction)
  : OTIMOFElement.OTIUMLReadIsClassifiedObjectAction
  = toOTIUMLReadIsClassifiedObjectAction(u, Option.empty[Document[Uml]])

  def toOTIUMLReadIsClassifiedObjectAction
  (u: UMLReadIsClassifiedObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReadIsClassifiedObjectAction
  = OTIMOFElement.OTIUMLReadIsClassifiedObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isDirect = u.isDirect,
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReadLinkAction
  (u: Uml#ReadLinkAction)
  : OTIMOFElement.OTIUMLReadLinkAction
  = toOTIUMLReadLinkAction(u, Option.empty[Document[Uml]])

  def toOTIUMLReadLinkAction
  (u: UMLReadLinkAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReadLinkAction
  = OTIMOFElement.OTIUMLReadLinkAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReadLinkObjectEndAction
  (u: Uml#ReadLinkObjectEndAction)
  : OTIMOFElement.OTIUMLReadLinkObjectEndAction
  = toOTIUMLReadLinkObjectEndAction(u, Option.empty[Document[Uml]])

  def toOTIUMLReadLinkObjectEndAction
  (u: UMLReadLinkObjectEndAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReadLinkObjectEndAction
  = OTIMOFElement.OTIUMLReadLinkObjectEndAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReadLinkObjectEndQualifierAction
  (u: Uml#ReadLinkObjectEndQualifierAction)
  : OTIMOFElement.OTIUMLReadLinkObjectEndQualifierAction
  = toOTIUMLReadLinkObjectEndQualifierAction(u, Option.empty[Document[Uml]])

  def toOTIUMLReadLinkObjectEndQualifierAction
  (u: UMLReadLinkObjectEndQualifierAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReadLinkObjectEndQualifierAction
  = OTIMOFElement.OTIUMLReadLinkObjectEndQualifierAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReadSelfAction
  (u: Uml#ReadSelfAction)
  : OTIMOFElement.OTIUMLReadSelfAction
  = toOTIUMLReadSelfAction(u, Option.empty[Document[Uml]])

  def toOTIUMLReadSelfAction
  (u: UMLReadSelfAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReadSelfAction
  = OTIMOFElement.OTIUMLReadSelfAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReadStructuralFeatureAction
  (u: Uml#ReadStructuralFeatureAction)
  : OTIMOFElement.OTIUMLReadStructuralFeatureAction
  = toOTIUMLReadStructuralFeatureAction(u, Option.empty[Document[Uml]])

  def toOTIUMLReadStructuralFeatureAction
  (u: UMLReadStructuralFeatureAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReadStructuralFeatureAction
  = OTIMOFElement.OTIUMLReadStructuralFeatureAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReadVariableAction
  (u: Uml#ReadVariableAction)
  : OTIMOFElement.OTIUMLReadVariableAction
  = toOTIUMLReadVariableAction(u, Option.empty[Document[Uml]])

  def toOTIUMLReadVariableAction
  (u: UMLReadVariableAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReadVariableAction
  = OTIMOFElement.OTIUMLReadVariableAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLRealization
  (u: Uml#Realization)
  : OTIMOFElement.OTIUMLRealization
  = toOTIUMLRealization(u, Option.empty[Document[Uml]])

  def toOTIUMLRealization
  (u: UMLRealization[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLRealization
  = OTIMOFElement.OTIUMLRealization(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReception
  (u: Uml#Reception)
  : OTIMOFElement.OTIUMLReception
  = toOTIUMLReception(u, Option.empty[Document[Uml]])

  def toOTIUMLReception
  (u: UMLReception[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReception
  = OTIMOFElement.OTIUMLReception(
     otiMOFElementLocation = getElementLocationOf(u, context),
     concurrency = u.concurrency,
     isAbstract = u.isAbstract,
     isLeaf = u.isLeaf,
     isStatic = u.isStatic,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReclassifyObjectAction
  (u: Uml#ReclassifyObjectAction)
  : OTIMOFElement.OTIUMLReclassifyObjectAction
  = toOTIUMLReclassifyObjectAction(u, Option.empty[Document[Uml]])

  def toOTIUMLReclassifyObjectAction
  (u: UMLReclassifyObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReclassifyObjectAction
  = OTIMOFElement.OTIUMLReclassifyObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isReplaceAll = u.isReplaceAll,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLRedefinableTemplateSignature
  (u: Uml#RedefinableTemplateSignature)
  : OTIMOFElement.OTIUMLRedefinableTemplateSignature
  = toOTIUMLRedefinableTemplateSignature(u, Option.empty[Document[Uml]])

  def toOTIUMLRedefinableTemplateSignature
  (u: UMLRedefinableTemplateSignature[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLRedefinableTemplateSignature
  = OTIMOFElement.OTIUMLRedefinableTemplateSignature(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReduceAction
  (u: Uml#ReduceAction)
  : OTIMOFElement.OTIUMLReduceAction
  = toOTIUMLReduceAction(u, Option.empty[Document[Uml]])

  def toOTIUMLReduceAction
  (u: UMLReduceAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReduceAction
  = OTIMOFElement.OTIUMLReduceAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isOrdered = u.isOrdered,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLRegion
  (u: Uml#Region)
  : OTIMOFElement.OTIUMLRegion
  = toOTIUMLRegion(u, Option.empty[Document[Uml]])

  def toOTIUMLRegion
  (u: UMLRegion[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLRegion
  = OTIMOFElement.OTIUMLRegion(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLRemoveStructuralFeatureValueAction
  (u: Uml#RemoveStructuralFeatureValueAction)
  : OTIMOFElement.OTIUMLRemoveStructuralFeatureValueAction
  = toOTIUMLRemoveStructuralFeatureValueAction(u, Option.empty[Document[Uml]])

  def toOTIUMLRemoveStructuralFeatureValueAction
  (u: UMLRemoveStructuralFeatureValueAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLRemoveStructuralFeatureValueAction
  = OTIMOFElement.OTIUMLRemoveStructuralFeatureValueAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isRemoveDuplicates = u.isRemoveDuplicates,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLRemoveVariableValueAction
  (u: Uml#RemoveVariableValueAction)
  : OTIMOFElement.OTIUMLRemoveVariableValueAction
  = toOTIUMLRemoveVariableValueAction(u, Option.empty[Document[Uml]])

  def toOTIUMLRemoveVariableValueAction
  (u: UMLRemoveVariableValueAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLRemoveVariableValueAction
  = OTIMOFElement.OTIUMLRemoveVariableValueAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isRemoveDuplicates = u.isRemoveDuplicates,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLReplyAction
  (u: Uml#ReplyAction)
  : OTIMOFElement.OTIUMLReplyAction
  = toOTIUMLReplyAction(u, Option.empty[Document[Uml]])

  def toOTIUMLReplyAction
  (u: UMLReplyAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLReplyAction
  = OTIMOFElement.OTIUMLReplyAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLSendObjectAction
  (u: Uml#SendObjectAction)
  : OTIMOFElement.OTIUMLSendObjectAction
  = toOTIUMLSendObjectAction(u, Option.empty[Document[Uml]])

  def toOTIUMLSendObjectAction
  (u: UMLSendObjectAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLSendObjectAction
  = OTIMOFElement.OTIUMLSendObjectAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLSendSignalAction
  (u: Uml#SendSignalAction)
  : OTIMOFElement.OTIUMLSendSignalAction
  = toOTIUMLSendSignalAction(u, Option.empty[Document[Uml]])

  def toOTIUMLSendSignalAction
  (u: UMLSendSignalAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLSendSignalAction
  = OTIMOFElement.OTIUMLSendSignalAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLSequenceNode
  (u: Uml#SequenceNode)
  : OTIMOFElement.OTIUMLSequenceNode
  = toOTIUMLSequenceNode(u, Option.empty[Document[Uml]])

  def toOTIUMLSequenceNode
  (u: UMLSequenceNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLSequenceNode
  = OTIMOFElement.OTIUMLSequenceNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     mustIsolate = u.mustIsolate,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLSignal
  (u: Uml#Signal)
  : OTIMOFElement.OTIUMLSignal
  = toOTIUMLSignal(u, Option.empty[Document[Uml]])

  def toOTIUMLSignal
  (u: UMLSignal[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLSignal
  = OTIMOFElement.OTIUMLSignal(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLSignalEvent
  (u: Uml#SignalEvent)
  : OTIMOFElement.OTIUMLSignalEvent
  = toOTIUMLSignalEvent(u, Option.empty[Document[Uml]])

  def toOTIUMLSignalEvent
  (u: UMLSignalEvent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLSignalEvent
  = OTIMOFElement.OTIUMLSignalEvent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLSlot
  (u: Uml#Slot)
  : OTIMOFElement.OTIUMLSlot
  = toOTIUMLSlot(u, Option.empty[Document[Uml]])

  def toOTIUMLSlot
  (u: UMLSlot[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLSlot
  = OTIMOFElement.OTIUMLSlot(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLStartClassifierBehaviorAction
  (u: Uml#StartClassifierBehaviorAction)
  : OTIMOFElement.OTIUMLStartClassifierBehaviorAction
  = toOTIUMLStartClassifierBehaviorAction(u, Option.empty[Document[Uml]])

  def toOTIUMLStartClassifierBehaviorAction
  (u: UMLStartClassifierBehaviorAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLStartClassifierBehaviorAction
  = OTIMOFElement.OTIUMLStartClassifierBehaviorAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLStartObjectBehaviorAction
  (u: Uml#StartObjectBehaviorAction)
  : OTIMOFElement.OTIUMLStartObjectBehaviorAction
  = toOTIUMLStartObjectBehaviorAction(u, Option.empty[Document[Uml]])

  def toOTIUMLStartObjectBehaviorAction
  (u: UMLStartObjectBehaviorAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLStartObjectBehaviorAction
  = OTIMOFElement.OTIUMLStartObjectBehaviorAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     isSynchronous = u.isSynchronous,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLState
  (u: Uml#State)
  : OTIMOFElement.OTIUMLState
  = toOTIUMLState(u, Option.empty[Document[Uml]])

  def toOTIUMLState
  (u: UMLState[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLState
  = OTIMOFElement.OTIUMLState(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLStateInvariant
  (u: Uml#StateInvariant)
  : OTIMOFElement.OTIUMLStateInvariant
  = toOTIUMLStateInvariant(u, Option.empty[Document[Uml]])

  def toOTIUMLStateInvariant
  (u: UMLStateInvariant[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLStateInvariant
  = OTIMOFElement.OTIUMLStateInvariant(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLStateMachine
  (u: Uml#StateMachine)
  : OTIMOFElement.OTIUMLStateMachine
  = toOTIUMLStateMachine(u, Option.empty[Document[Uml]])

  def toOTIUMLStateMachine
  (u: UMLStateMachine[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLStateMachine
  = OTIMOFElement.OTIUMLStateMachine(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     isReentrant = u.isReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLStereotype
  (u: Uml#Stereotype)
  : OTIMOFElement.OTIUMLStereotype
  = toOTIUMLStereotype(u, Option.empty[Document[Uml]])

  def toOTIUMLStereotype
  (u: UMLStereotype[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLStereotype
  = OTIMOFElement.OTIUMLStereotype(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isActive = u.isActive,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLStringExpression
  (u: Uml#StringExpression)
  : OTIMOFElement.OTIUMLStringExpression
  = toOTIUMLStringExpression(u, Option.empty[Document[Uml]])

  def toOTIUMLStringExpression
  (u: UMLStringExpression[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLStringExpression
  = OTIMOFElement.OTIUMLStringExpression(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     symbol = u.symbol,
     visibility = u.visibility)


  implicit def toOTIUMLStructuredActivityNode
  (u: Uml#StructuredActivityNode)
  : OTIMOFElement.OTIUMLStructuredActivityNode
  = toOTIUMLStructuredActivityNode(u, Option.empty[Document[Uml]])

  def toOTIUMLStructuredActivityNode
  (u: UMLStructuredActivityNode[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLStructuredActivityNode
  = OTIMOFElement.OTIUMLStructuredActivityNode(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     mustIsolate = u.mustIsolate,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLSubstitution
  (u: Uml#Substitution)
  : OTIMOFElement.OTIUMLSubstitution
  = toOTIUMLSubstitution(u, Option.empty[Document[Uml]])

  def toOTIUMLSubstitution
  (u: UMLSubstitution[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLSubstitution
  = OTIMOFElement.OTIUMLSubstitution(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLTemplateBinding
  (u: Uml#TemplateBinding)
  : OTIMOFElement.OTIUMLTemplateBinding
  = toOTIUMLTemplateBinding(u, Option.empty[Document[Uml]])

  def toOTIUMLTemplateBinding
  (u: UMLTemplateBinding[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTemplateBinding
  = OTIMOFElement.OTIUMLTemplateBinding(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLTemplateParameter
  (u: Uml#TemplateParameter)
  : OTIMOFElement.OTIUMLTemplateParameter
  = toOTIUMLTemplateParameter(u, Option.empty[Document[Uml]])

  def toOTIUMLTemplateParameter
  (u: UMLTemplateParameter[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTemplateParameter
  = OTIMOFElement.OTIUMLTemplateParameter(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLTemplateParameterSubstitution
  (u: Uml#TemplateParameterSubstitution)
  : OTIMOFElement.OTIUMLTemplateParameterSubstitution
  = toOTIUMLTemplateParameterSubstitution(u, Option.empty[Document[Uml]])

  def toOTIUMLTemplateParameterSubstitution
  (u: UMLTemplateParameterSubstitution[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTemplateParameterSubstitution
  = OTIMOFElement.OTIUMLTemplateParameterSubstitution(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLTemplateSignature
  (u: Uml#TemplateSignature)
  : OTIMOFElement.OTIUMLTemplateSignature
  = toOTIUMLTemplateSignature(u, Option.empty[Document[Uml]])

  def toOTIUMLTemplateSignature
  (u: UMLTemplateSignature[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTemplateSignature
  = OTIMOFElement.OTIUMLTemplateSignature(
     otiMOFElementLocation = getElementLocationOf(u, context))


  implicit def toOTIUMLTestIdentityAction
  (u: Uml#TestIdentityAction)
  : OTIMOFElement.OTIUMLTestIdentityAction
  = toOTIUMLTestIdentityAction(u, Option.empty[Document[Uml]])

  def toOTIUMLTestIdentityAction
  (u: UMLTestIdentityAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTestIdentityAction
  = OTIMOFElement.OTIUMLTestIdentityAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLTimeConstraint
  (u: Uml#TimeConstraint)
  : OTIMOFElement.OTIUMLTimeConstraint
  = toOTIUMLTimeConstraint(u, Option.empty[Document[Uml]])

  def toOTIUMLTimeConstraint
  (u: UMLTimeConstraint[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTimeConstraint
  = OTIMOFElement.OTIUMLTimeConstraint(
     otiMOFElementLocation = getElementLocationOf(u, context),
     firstEvent = u.firstEvent,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLTimeEvent
  (u: Uml#TimeEvent)
  : OTIMOFElement.OTIUMLTimeEvent
  = toOTIUMLTimeEvent(u, Option.empty[Document[Uml]])

  def toOTIUMLTimeEvent
  (u: UMLTimeEvent[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTimeEvent
  = OTIMOFElement.OTIUMLTimeEvent(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isRelative = u.isRelative,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLTimeExpression
  (u: Uml#TimeExpression)
  : OTIMOFElement.OTIUMLTimeExpression
  = toOTIUMLTimeExpression(u, Option.empty[Document[Uml]])

  def toOTIUMLTimeExpression
  (u: UMLTimeExpression[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTimeExpression
  = OTIMOFElement.OTIUMLTimeExpression(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLTimeInterval
  (u: Uml#TimeInterval)
  : OTIMOFElement.OTIUMLTimeInterval
  = toOTIUMLTimeInterval(u, Option.empty[Document[Uml]])

  def toOTIUMLTimeInterval
  (u: UMLTimeInterval[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTimeInterval
  = OTIMOFElement.OTIUMLTimeInterval(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLTimeObservation
  (u: Uml#TimeObservation)
  : OTIMOFElement.OTIUMLTimeObservation
  = toOTIUMLTimeObservation(u, Option.empty[Document[Uml]])

  def toOTIUMLTimeObservation
  (u: UMLTimeObservation[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTimeObservation
  = OTIMOFElement.OTIUMLTimeObservation(
     otiMOFElementLocation = getElementLocationOf(u, context),
     firstEvent = u.firstEvent,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLTransition
  (u: Uml#Transition)
  : OTIMOFElement.OTIUMLTransition
  = toOTIUMLTransition(u, Option.empty[Document[Uml]])

  def toOTIUMLTransition
  (u: UMLTransition[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTransition
  = OTIMOFElement.OTIUMLTransition(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     kind = u.kind,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLTrigger
  (u: Uml#Trigger)
  : OTIMOFElement.OTIUMLTrigger
  = toOTIUMLTrigger(u, Option.empty[Document[Uml]])

  def toOTIUMLTrigger
  (u: UMLTrigger[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLTrigger
  = OTIMOFElement.OTIUMLTrigger(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLUnmarshallAction
  (u: Uml#UnmarshallAction)
  : OTIMOFElement.OTIUMLUnmarshallAction
  = toOTIUMLUnmarshallAction(u, Option.empty[Document[Uml]])

  def toOTIUMLUnmarshallAction
  (u: UMLUnmarshallAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLUnmarshallAction
  = OTIMOFElement.OTIUMLUnmarshallAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLUsage
  (u: Uml#Usage)
  : OTIMOFElement.OTIUMLUsage
  = toOTIUMLUsage(u, Option.empty[Document[Uml]])

  def toOTIUMLUsage
  (u: UMLUsage[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLUsage
  = OTIMOFElement.OTIUMLUsage(
     otiMOFElementLocation = getElementLocationOf(u, context),
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLUseCase
  (u: Uml#UseCase)
  : OTIMOFElement.OTIUMLUseCase
  = toOTIUMLUseCase(u, Option.empty[Document[Uml]])

  def toOTIUMLUseCase
  (u: UMLUseCase[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLUseCase
  = OTIMOFElement.OTIUMLUseCase(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isAbstract = u.isAbstract,
     isFinalSpecialization = u.isFinalSpecialization,
     isLeaf = u.isLeaf,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLValuePin
  (u: Uml#ValuePin)
  : OTIMOFElement.OTIUMLValuePin
  = toOTIUMLValuePin(u, Option.empty[Document[Uml]])

  def toOTIUMLValuePin
  (u: UMLValuePin[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLValuePin
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


  implicit def toOTIUMLValueSpecificationAction
  (u: Uml#ValueSpecificationAction)
  : OTIMOFElement.OTIUMLValueSpecificationAction
  = toOTIUMLValueSpecificationAction(u, Option.empty[Document[Uml]])

  def toOTIUMLValueSpecificationAction
  (u: UMLValueSpecificationAction[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLValueSpecificationAction
  = OTIMOFElement.OTIUMLValueSpecificationAction(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isLeaf = u.isLeaf,
     isLocallyReentrant = u.isLocallyReentrant,
     name = u.name,
     visibility = u.visibility)


  implicit def toOTIUMLVariable
  (u: Uml#Variable)
  : OTIMOFElement.OTIUMLVariable
  = toOTIUMLVariable(u, Option.empty[Document[Uml]])

  def toOTIUMLVariable
  (u: UMLVariable[Uml],
   context: Option[Document[Uml]] = None)
  : OTIMOFElement.OTIUMLVariable
  = OTIMOFElement.OTIUMLVariable(
     otiMOFElementLocation = getElementLocationOf(u, context),
     isOrdered = u.isOrdered,
     isUnique = u.isUnique,
     name = u.name,
     visibility = u.visibility)
}
