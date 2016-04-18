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
import org.omg.oti.json.common.ToolSpecificElementDocumentURL
import org.omg.oti.json.common.OTIPrimitiveTypes.TOOL_SPECIFIC_ID
import org.omg.oti.json.uml._
import org.omg.oti.json.uml.OTIMOFLink._
import org.omg.oti.json.extent._
import org.omg.oti.uml.canonicalXMI.{DocumentOps,DocumentSet}
import org.omg.oti.uml.canonicalXMI.helper.OTIDocumentSetAdapter
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.write.api.{UMLFactory, UMLUpdate}

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
  def toCompositeLinkExtent[U <: UMLElement[Uml], V <: UMLElement[Uml]]
  (u : U, v : V, ctor: (ToolSpecificElementDocumentURL, ToolSpecificElementDocumentURL) => OTIMOFCompositeLink)
  : OTIMOFCompositeLink
  = ctor(
    ToolSpecificElementDocumentURL(u.toolSpecific_id, u.toolSpecific_url),
    ToolSpecificElementDocumentURL(v.toolSpecific_id, v.toolSpecific_url))
  // <!-- End of user code additions -->

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

  implicit def optionToOTI[U,V]
  (value: Option[U])
  (implicit u2v: U => V)
  : Option[V]
  = value.map(u2v)

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLAbstraction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLAbstraction(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.mapping, OTIUMLA_mapping_abstraction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLAcceptCallAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLAcceptCallAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isUnmarshall = u.isUnmarshall,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.result, OTIUMLA_result_acceptEventAction)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.returnInformation, OTIUMLA_returnInformation_acceptCallAction)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.trigger, OTIUMLA_trigger_acceptEventAction)
    e9 <-
      toReferenceLinkExtent(e8, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e9  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLAcceptEventAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLAcceptEventAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isUnmarshall = u.isUnmarshall,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.result, OTIUMLA_result_acceptEventAction)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.trigger, OTIUMLA_trigger_acceptEventAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLActionExecutionSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLActionExecutionSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <-
      toReferenceLinkExtent(e3, u, u.covered, OTIUMLA_covered_coveredBy)
    result = e4  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLActionInputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.fromAction, OTIUMLA_fromAction_actionInputPin)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLActivity[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.edge, OTIUMLA_edge_activity)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.generalization, OTIUMLA_generalization_specific)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.group, OTIUMLA_group_inActivity)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e8 <- 
      toCompositeFirstEndOrderedLinkExtent(e7, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.node, OTIUMLA_node_activity)
    e10 <- 
      toCompositeFirstEndOrderedLinkExtent(e9, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e14 <- 
      toCompositeFirstEndOrderedLinkExtent(e13, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e15 <- 
      toCompositeFirstEndOrderedLinkExtent(e14, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e20 <- 
      toCompositeLinkExtent(e19, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e21 <- 
      toCompositeLinkExtent(e20, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e22 <- 
      toCompositeLinkExtent(e21, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e23 <- 
      toCompositeLinkExtent(e22, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e24 <- 
      toCompositeLinkExtent(e23, u, u.variable, OTIUMLA_variable_activityScope)
    e25 <-
      toReferenceLinkExtent(e24, u, u.structuredNode, OTIUMLA_structuredNode_activity)
    e26 <-
      toReferenceLinkExtent(e25, u, u.useCase, OTIUMLA_subject_useCase)
    result = e26  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLActivityFinalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLActivityFinalNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <-
      toReferenceLinkExtent(e2, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLActivityParameterNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLActivityParameterNode(
            toolSpecific_id = u.toolSpecific_id,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    e4 <-
      toReferenceLinkExtent(e3, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e4  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLActivityPartition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLActivityPartition(
            toolSpecific_id = u.toolSpecific_id,
            isDimension = u.isDimension,
            isExternal = u.isExternal,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.subpartition, OTIUMLA_subpartition_superPartition)
    e4 <-
      toReferenceLinkExtent(e3, u, u.edge, OTIUMLA_edge_inPartition)
    e5 <-
      toReferenceLinkExtent(e4, u, u.node, OTIUMLA_inPartition_node)
    result = e5  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLActor[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLActor(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e14 <-
      toReferenceLinkExtent(e13, u, u.useCase, OTIUMLA_subject_useCase)
    result = e14  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLAddStructuralFeatureValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLAddStructuralFeatureValueAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isReplaceAll = u.isReplaceAll,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.insertAt, OTIUMLA_insertAt_addStructuralFeatureValueAction)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.object, OTIUMLA_object_structuralFeatureAction)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.result, OTIUMLA_result_writeStructuralFeatureAction)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.value, OTIUMLA_value_writeStructuralFeatureAction)
    e10 <-
      toReferenceLinkExtent(e9, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e10  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLAddVariableValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLAddVariableValueAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isReplaceAll = u.isReplaceAll,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.insertAt, OTIUMLA_insertAt_addVariableValueAction)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.value, OTIUMLA_value_writeVariableAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLAnyReceiveEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLAnyReceiveEvent(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLArtifact[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLArtifact(
            toolSpecific_id = u.toolSpecific_id,
            fileName = u.fileName,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.manifestation, OTIUMLA_manifestation_artifact)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nestedArtifact, OTIUMLA_nestedArtifact_artifact)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.ownedAttribute, OTIUMLA_ownedAttribute_artifact)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e9 <- 
      toCompositeFirstEndOrderedLinkExtent(e8, u, u.ownedOperation, OTIUMLA_ownedOperation_artifact)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e16 <-
      toReferenceLinkExtent(e15, u, u.useCase, OTIUMLA_subject_useCase)
    result = e16  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLAssociation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLAssociation(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isDerived = u.isDerived,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.ownedEnd, OTIUMLA_ownedEnd_owningAssociation)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e13 <-
      toReferenceLinkExtent(e12, u, u.useCase, OTIUMLA_subject_useCase)
    result = e13  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLAssociationClass[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLAssociationClass(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isDerived = u.isDerived,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e11 <- 
      toCompositeFirstEndOrderedLinkExtent(e10, u, u.ownedEnd, OTIUMLA_ownedEnd_owningAssociation)
    e12 <- 
      toCompositeFirstEndOrderedLinkExtent(e11, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e20 <-
      toReferenceLinkExtent(e19, u, u.useCase, OTIUMLA_subject_useCase)
    result = e20  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLBehaviorExecutionSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLBehaviorExecutionSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <-
      toReferenceLinkExtent(e3, u, u.covered, OTIUMLA_covered_coveredBy)
    result = e4  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLBroadcastSignalAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLBroadcastSignalAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeFirstEndOrderedLinkExtent(e0, u, u.argument, OTIUMLA_argument_invocationAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLCallBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLCallBehaviorAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isSynchronous = u.isSynchronous,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeFirstEndOrderedLinkExtent(e0, u, u.argument, OTIUMLA_argument_invocationAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.result, OTIUMLA_result_callAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLCallEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLCallEvent(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLCallOperationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLCallOperationAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isSynchronous = u.isSynchronous,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeFirstEndOrderedLinkExtent(e0, u, u.argument, OTIUMLA_argument_invocationAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.result, OTIUMLA_result_callAction)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.target, OTIUMLA_target_callOperationAction)
    e9 <-
      toReferenceLinkExtent(e8, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e9  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLCentralBufferNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLCentralBufferNode(
            toolSpecific_id = u.toolSpecific_id,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    e4 <-
      toReferenceLinkExtent(e3, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e4  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLChangeEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLChangeEvent(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.changeExpression, OTIUMLA_changeExpression_changeEvent)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLClass[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLClass(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e11 <- 
      toCompositeFirstEndOrderedLinkExtent(e10, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e19 <-
      toReferenceLinkExtent(e18, u, u.useCase, OTIUMLA_subject_useCase)
    result = e19  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLClassifierTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLClassifierTemplateParameter(
            toolSpecific_id = u.toolSpecific_id,
            allowSubstitutable = u.allowSubstitutable))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter)
    e4 <-
      toReferenceLinkExtent(e3, u, u.parameteredElement, OTIUMLA_classifier_templateParameter_parameteredElement)
    result = e4  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLClause[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLClause(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <-
      toReferenceLinkExtent(e1, u, u.predecessorClause, OTIUMLA_predecessorClause_successorClause)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLClearAssociationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLClearAssociationAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.object, OTIUMLA_object_clearAssociationAction)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLClearStructuralFeatureAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLClearStructuralFeatureAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.object, OTIUMLA_object_structuralFeatureAction)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.result, OTIUMLA_result_clearStructuralFeatureAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLClearVariableAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLClearVariableAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <-
      toReferenceLinkExtent(e5, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e6  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLCollaboration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLCollaboration(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.ownedAttribute, OTIUMLA_ownedAttribute_structuredClassifier)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e16 <-
      toReferenceLinkExtent(e15, u, u.useCase, OTIUMLA_subject_useCase)
    result = e16  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLCollaborationUse[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLCollaborationUse(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.roleBinding, OTIUMLA_roleBinding_collaborationUse)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLCombinedFragment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLCombinedFragment(
            toolSpecific_id = u.toolSpecific_id,
            interactionOperator = u.interactionOperator,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.cfragmentGate, OTIUMLA_cfragmentGate_combinedFragment)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e4 <- 
      toCompositeFirstEndOrderedLinkExtent(e3, u, u.operand, OTIUMLA_operand_combinedFragment)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <-
      toReferenceLinkExtent(e5, u, u.covered, OTIUMLA_covered_coveredBy)
    result = e6  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLComment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLComment(
            toolSpecific_id = u.toolSpecific_id,
            body = u.body))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e1  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLCommunicationPath[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLCommunicationPath(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isDerived = u.isDerived,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.ownedEnd, OTIUMLA_ownedEnd_owningAssociation)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e13 <-
      toReferenceLinkExtent(e12, u, u.useCase, OTIUMLA_subject_useCase)
    result = e13  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLComponent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLComponent(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isIndirectlyInstantiated = u.isIndirectlyInstantiated,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e11 <- 
      toCompositeFirstEndOrderedLinkExtent(e10, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.packagedElement, OTIUMLA_packagedElement_component)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.realization, OTIUMLA_realization_abstraction_component)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e20 <- 
      toCompositeLinkExtent(e19, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e21 <-
      toReferenceLinkExtent(e20, u, u.useCase, OTIUMLA_subject_useCase)
    result = e21  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLComponentRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLComponentRealization(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.mapping, OTIUMLA_mapping_abstraction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLConditionalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLConditionalNode(
            toolSpecific_id = u.toolSpecific_id,
            isAssured = u.isAssured,
            isDeterminate = u.isDeterminate,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            mustIsolate = u.mustIsolate,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.clause, OTIUMLA_clause_conditionalNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.edge, OTIUMLA_edge_inStructuredNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.handler, OTIUMLA_handler_protectedNode)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.node, OTIUMLA_node_inStructuredNode)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e12 <- 
      toCompositeFirstEndOrderedLinkExtent(e11, u, u.result, OTIUMLA_result_conditionalNode)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.variable, OTIUMLA_variable_scope)
    e15 <-
      toReferenceLinkExtent(e14, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e15  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLConnectableElementTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLConnectableElementTemplateParameter(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLConnectionPointReference[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLConnectionPointReference(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLConnector[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLConnector(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isStatic = u.isStatic,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeFirstEndOrderedLinkExtent(e0, u, u.end, OTIUMLA_end_connector)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLConnectorEnd[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLConnectorEnd(
            toolSpecific_id = u.toolSpecific_id,
            isOrdered = u.isOrdered,
            isUnique = u.isUnique))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLConsiderIgnoreFragment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLConsiderIgnoreFragment(
            toolSpecific_id = u.toolSpecific_id,
            interactionOperator = u.interactionOperator,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.cfragmentGate, OTIUMLA_cfragmentGate_combinedFragment)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e4 <- 
      toCompositeFirstEndOrderedLinkExtent(e3, u, u.operand, OTIUMLA_operand_combinedFragment)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <-
      toReferenceLinkExtent(e5, u, u.covered, OTIUMLA_covered_coveredBy)
    result = e6  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLConstraint(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.specification, OTIUMLA_specification_owningConstraint)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLContinuation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLContinuation(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            setting = u.setting,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <-
      toReferenceLinkExtent(e3, u, u.covered, OTIUMLA_covered_coveredBy)
    result = e4  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLControlFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLControlFlow(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.guard, OTIUMLA_guard_activityEdge)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.weight, OTIUMLA_weight_activityEdge)
    e5 <-
      toReferenceLinkExtent(e4, u, u.interrupts, OTIUMLA_interruptingEdge_interrupts)
    e6 <-
      toReferenceLinkExtent(e5, u, u.source, OTIUMLA_outgoing_source_node)
    e7 <-
      toReferenceLinkExtent(e6, u, u.target, OTIUMLA_incoming_target_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLCreateLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLCreateLinkAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.endData, OTIUMLA_endData_createLinkAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.inputValue, OTIUMLA_inputValue_linkAction)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLCreateLinkObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLCreateLinkObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.endData, OTIUMLA_endData_createLinkAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.inputValue, OTIUMLA_inputValue_linkAction)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.result, OTIUMLA_result_createLinkObjectAction)
    e9 <-
      toReferenceLinkExtent(e8, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e9  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLCreateObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLCreateObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.result, OTIUMLA_result_createObjectAction)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDataStoreNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDataStoreNode(
            toolSpecific_id = u.toolSpecific_id,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    e4 <-
      toReferenceLinkExtent(e3, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e4  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDataType[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDataType(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeFirstEndOrderedLinkExtent(e4, u, u.ownedAttribute, OTIUMLA_ownedAttribute_datatype)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.ownedOperation, OTIUMLA_ownedOperation_datatype)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e14 <-
      toReferenceLinkExtent(e13, u, u.useCase, OTIUMLA_subject_useCase)
    result = e14  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDecisionNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDecisionNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <-
      toReferenceLinkExtent(e2, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDependency[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDependency(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDeployment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDeployment(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.configuration, OTIUMLA_configuration_deployment)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDeploymentSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.manifestation, OTIUMLA_manifestation_artifact)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nestedArtifact, OTIUMLA_nestedArtifact_artifact)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.ownedAttribute, OTIUMLA_ownedAttribute_artifact)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e9 <- 
      toCompositeFirstEndOrderedLinkExtent(e8, u, u.ownedOperation, OTIUMLA_ownedOperation_artifact)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e16 <-
      toReferenceLinkExtent(e15, u, u.useCase, OTIUMLA_subject_useCase)
    result = e16  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDestroyLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDestroyLinkAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.endData, OTIUMLA_endData_destroyLinkAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.inputValue, OTIUMLA_inputValue_linkAction)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDestroyObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDestroyObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isDestroyLinks = u.isDestroyLinks,
            isDestroyOwnedObjects = u.isDestroyOwnedObjects,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.target, OTIUMLA_target_destroyObjectAction)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDestructionOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDestructionOccurrenceSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDevice[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDevice(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.deployment, OTIUMLA_deployment_location)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.generalization, OTIUMLA_generalization_specific)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.nestedNode, OTIUMLA_nestedNode_node)
    e9 <- 
      toCompositeFirstEndOrderedLinkExtent(e8, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e13 <- 
      toCompositeFirstEndOrderedLinkExtent(e12, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e20 <- 
      toCompositeLinkExtent(e19, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e21 <-
      toReferenceLinkExtent(e20, u, u.useCase, OTIUMLA_subject_useCase)
    result = e21  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDuration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDuration(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.expr, OTIUMLA_expr_duration)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDurationConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDurationConstraint(
            toolSpecific_id = u.toolSpecific_id,
            firstEvent = u.firstEvent,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.specification, OTIUMLA_specification_durationConstraint)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDurationInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDurationInterval(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLDurationObservation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLDurationObservation(
            toolSpecific_id = u.toolSpecific_id,
            firstEvent = u.firstEvent,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLElementImport[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLElementImport(
            toolSpecific_id = u.toolSpecific_id,
            alias = u.alias,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e1  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLEnumeration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLEnumeration(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeFirstEndOrderedLinkExtent(e4, u, u.ownedAttribute, OTIUMLA_ownedAttribute_datatype)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.ownedLiteral, OTIUMLA_ownedLiteral_enumeration)
    e8 <- 
      toCompositeFirstEndOrderedLinkExtent(e7, u, u.ownedOperation, OTIUMLA_ownedOperation_datatype)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e15 <-
      toReferenceLinkExtent(e14, u, u.useCase, OTIUMLA_subject_useCase)
    result = e15  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLEnumerationLiteral[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLEnumerationLiteral(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.deployment, OTIUMLA_deployment_location)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.slot, OTIUMLA_slot_owningInstance)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.specification, OTIUMLA_specification_owningInstanceSpec)
    result = e5  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLExceptionHandler[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLExceptionHandler(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e1  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLExecutionEnvironment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLExecutionEnvironment(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.deployment, OTIUMLA_deployment_location)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.generalization, OTIUMLA_generalization_specific)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.nestedNode, OTIUMLA_nestedNode_node)
    e9 <- 
      toCompositeFirstEndOrderedLinkExtent(e8, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e13 <- 
      toCompositeFirstEndOrderedLinkExtent(e12, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e20 <- 
      toCompositeLinkExtent(e19, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e21 <-
      toReferenceLinkExtent(e20, u, u.useCase, OTIUMLA_subject_useCase)
    result = e21  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLExecutionOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLExecutionOccurrenceSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLExpansionNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLExpansionNode(
            toolSpecific_id = u.toolSpecific_id,
            isControlType = u.isControlType,
            isLeaf = u.isLeaf,
            name = u.name,
            ordering = u.ordering,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    e4 <-
      toReferenceLinkExtent(e3, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    e5 <-
      toReferenceLinkExtent(e4, u, u.regionAsInput, OTIUMLA_inputElement_regionAsInput)
    e6 <-
      toReferenceLinkExtent(e5, u, u.regionAsOutput, OTIUMLA_outputElement_regionAsOutput)
    result = e6  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLExpansionRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLExpansionRegion(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            mode = u.mode,
            mustIsolate = u.mustIsolate,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.edge, OTIUMLA_edge_inStructuredNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.handler, OTIUMLA_handler_protectedNode)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.node, OTIUMLA_node_inStructuredNode)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.structuredNodeOutput, OTIUMLA_structuredNodeOutput_structuredActivityNode)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.variable, OTIUMLA_variable_scope)
    e14 <-
      toReferenceLinkExtent(e13, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e14  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLExpression(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            symbol = u.symbol,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeFirstEndOrderedLinkExtent(e1, u, u.operand, OTIUMLA_operand_expression)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLExtend[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLExtend(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.condition, OTIUMLA_condition_extend)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLExtension[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLExtension(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isDerived = u.isDerived,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedEnd, OTIUMLA_ownedEnd_extension)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e13 <-
      toReferenceLinkExtent(e12, u, u.useCase, OTIUMLA_subject_useCase)
    result = e13  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLExtensionEnd[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.defaultValue, OTIUMLA_defaultValue_owningProperty)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.deployment, OTIUMLA_deployment_location)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.qualifier, OTIUMLA_qualifier_associationEnd)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    e8 <-
      toReferenceSecondEndOrderedLinkExtent(e7, u, u.association, OTIUMLA_memberEnd_association)
    e9 <-
      toReferenceLinkExtent(e8, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement)
    result = e9  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLExtensionPoint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLExtensionPoint(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLFinalState[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLFinalState(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.connection, OTIUMLA_connection_state)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.connectionPoint, OTIUMLA_connectionPoint_state)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.deferrableTrigger, OTIUMLA_deferrableTrigger_state)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.doActivity, OTIUMLA_doActivity_state)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.entry, OTIUMLA_entry_state)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.exit, OTIUMLA_exit_state)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.region, OTIUMLA_region_state)
    e13 <-
      toReferenceLinkExtent(e12, u, u.submachine, OTIUMLA_submachineState_submachine)
    result = e13  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLFlowFinalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLFlowFinalNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <-
      toReferenceLinkExtent(e2, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLForkNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLForkNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <-
      toReferenceLinkExtent(e2, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLFunctionBehavior[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e11 <- 
      toCompositeFirstEndOrderedLinkExtent(e10, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e12 <- 
      toCompositeFirstEndOrderedLinkExtent(e11, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e20 <- 
      toCompositeLinkExtent(e19, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e21 <-
      toReferenceLinkExtent(e20, u, u.useCase, OTIUMLA_subject_useCase)
    result = e21  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLGate[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLGate(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLGeneralOrdering[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLGeneralOrdering(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <-
      toReferenceLinkExtent(e2, u, u.after, OTIUMLA_toBefore_after)
    e4 <-
      toReferenceLinkExtent(e3, u, u.before, OTIUMLA_before_toAfter)
    result = e4  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLGeneralization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLGeneralization(
            toolSpecific_id = u.toolSpecific_id,
            isSubstitutable = u.isSubstitutable))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <-
      toReferenceLinkExtent(e1, u, u.generalizationSet, OTIUMLA_generalizationSet_generalization)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLGeneralizationSet[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLGeneralizationSet(
            toolSpecific_id = u.toolSpecific_id,
            isCovering = u.isCovering,
            isDisjoint = u.isDisjoint,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <-
      toReferenceLinkExtent(e2, u, u.powertype, OTIUMLA_powertypeExtent_powertype)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLImage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLImage(
            toolSpecific_id = u.toolSpecific_id,
            content = u.content,
            format = u.format,
            location = u.location))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e1  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInclude[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInclude(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInformationFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInformationFlow(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInformationItem[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInformationItem(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e12 <-
      toReferenceLinkExtent(e11, u, u.useCase, OTIUMLA_subject_useCase)
    result = e12  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInitialNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInitialNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <-
      toReferenceLinkExtent(e2, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    e6 <-
      toReferenceLinkExtent(e5, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e6  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInstanceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInstanceSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.deployment, OTIUMLA_deployment_location)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.slot, OTIUMLA_slot_owningInstance)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.specification, OTIUMLA_specification_owningInstanceSpec)
    result = e5  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInstanceValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInstanceValue(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInteraction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInteraction(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            isReentrant = u.isReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.action, OTIUMLA_action_interaction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.formalGate, OTIUMLA_formalGate_interaction)
    e5 <- 
      toCompositeFirstEndOrderedLinkExtent(e4, u, u.fragment, OTIUMLA_fragment_enclosingInteraction)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.generalization, OTIUMLA_generalization_specific)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.lifeline, OTIUMLA_lifeline_interaction)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.message, OTIUMLA_message_interaction)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e12 <- 
      toCompositeFirstEndOrderedLinkExtent(e11, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e13 <- 
      toCompositeFirstEndOrderedLinkExtent(e12, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e17 <- 
      toCompositeFirstEndOrderedLinkExtent(e16, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e18 <- 
      toCompositeFirstEndOrderedLinkExtent(e17, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    e20 <- 
      toCompositeLinkExtent(e19, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e21 <- 
      toCompositeLinkExtent(e20, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e22 <- 
      toCompositeLinkExtent(e21, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e23 <- 
      toCompositeLinkExtent(e22, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e24 <- 
      toCompositeLinkExtent(e23, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e25 <- 
      toCompositeLinkExtent(e24, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e26 <- 
      toCompositeLinkExtent(e25, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e27 <-
      toReferenceLinkExtent(e26, u, u.covered, OTIUMLA_covered_coveredBy)
    e28 <-
      toReferenceLinkExtent(e27, u, u.useCase, OTIUMLA_subject_useCase)
    result = e28  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInteractionConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInteractionConstraint(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.maxint, OTIUMLA_maxint_interactionConstraint)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.minint, OTIUMLA_minint_interactionConstraint)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.specification, OTIUMLA_specification_owningConstraint)
    result = e5  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInteractionOperand[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInteractionOperand(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e2 <- 
      toCompositeFirstEndOrderedLinkExtent(e1, u, u.fragment, OTIUMLA_fragment_enclosingOperand)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.guard, OTIUMLA_guard_interactionOperand)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e9 <-
      toReferenceLinkExtent(e8, u, u.covered, OTIUMLA_covered_coveredBy)
    result = e9  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInteractionUse[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInteractionUse(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.actualGate, OTIUMLA_actualGate_interactionUse)
    e2 <- 
      toCompositeFirstEndOrderedLinkExtent(e1, u, u.argument, OTIUMLA_argument_interactionUse)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.returnValue, OTIUMLA_returnValue_interactionUse)
    e7 <-
      toReferenceLinkExtent(e6, u, u.covered, OTIUMLA_covered_coveredBy)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInterface[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInterface(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeFirstEndOrderedLinkExtent(e4, u, u.nestedClassifier, OTIUMLA_nestedClassifier_interface)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.ownedAttribute, OTIUMLA_ownedAttribute_interface)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e8 <- 
      toCompositeFirstEndOrderedLinkExtent(e7, u, u.ownedOperation, OTIUMLA_ownedOperation_interface)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedReception, OTIUMLA_ownedReception_interface)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.protocol, OTIUMLA_protocol_interface)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e17 <-
      toReferenceLinkExtent(e16, u, u.useCase, OTIUMLA_subject_useCase)
    result = e17  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInterfaceRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInterfaceRealization(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.mapping, OTIUMLA_mapping_abstraction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInterruptibleActivityRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInterruptibleActivityRegion(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLInterval(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLIntervalConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLIntervalConstraint(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.specification, OTIUMLA_specification_intervalConstraint)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLJoinNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLJoinNode(
            toolSpecific_id = u.toolSpecific_id,
            isCombineDuplicate = u.isCombineDuplicate,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.joinSpec, OTIUMLA_joinSpec_joinNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <-
      toReferenceLinkExtent(e3, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e4  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLLifeline[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLLifeline(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.selector, OTIUMLA_selector_lifeline)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLLinkEndCreationData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLLinkEndCreationData(
            toolSpecific_id = u.toolSpecific_id,
            isReplaceAll = u.isReplaceAll))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.qualifier, OTIUMLA_qualifier_linkEndData)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLLinkEndData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLLinkEndData(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.qualifier, OTIUMLA_qualifier_linkEndData)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLLinkEndDestructionData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLLinkEndDestructionData(
            toolSpecific_id = u.toolSpecific_id,
            isDestroyDuplicates = u.isDestroyDuplicates))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.qualifier, OTIUMLA_qualifier_linkEndData)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLLiteralBoolean[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLLiteralBoolean(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            value = u.value,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLLiteralInteger[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLLiteralInteger(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            value = u.value,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLLiteralNull[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLLiteralNull(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLLiteralReal[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLLiteralReal(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            value = u.value,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLLiteralString[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLLiteralString(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            value = u.value,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLLiteralUnlimitedNatural[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLLiteralUnlimitedNatural(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            value = u.value,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLLoopNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLLoopNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isTestedFirst = u.isTestedFirst,
            mustIsolate = u.mustIsolate,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.edge, OTIUMLA_edge_inStructuredNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.handler, OTIUMLA_handler_protectedNode)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.loopVariable, OTIUMLA_loopVariable_loopNode)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.loopVariableInput, OTIUMLA_loopVariableInput_loopNode)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.node, OTIUMLA_node_inStructuredNode)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e13 <- 
      toCompositeFirstEndOrderedLinkExtent(e12, u, u.result, OTIUMLA_result_loopNode)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.variable, OTIUMLA_variable_scope)
    e15 <-
      toReferenceLinkExtent(e14, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e15  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLManifestation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLManifestation(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.mapping, OTIUMLA_mapping_abstraction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLMergeNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLMergeNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <-
      toReferenceLinkExtent(e2, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLMessage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLMessage(
            toolSpecific_id = u.toolSpecific_id,
            messageSort = u.messageSort,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeFirstEndOrderedLinkExtent(e0, u, u.argument, OTIUMLA_argument_message)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLMessageOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLMessageOccurrenceSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLModel[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLModel(
            toolSpecific_id = u.toolSpecific_id,
            URI = u.URI,
            name = u.name,
            viewpoint = u.viewpoint,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.packageMerge, OTIUMLA_packageMerge_receivingPackage)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.packagedElement, OTIUMLA_packagedElement_owningPackage)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.profileApplication, OTIUMLA_profileApplication_applyingPackage)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    result = e10  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLNode(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.deployment, OTIUMLA_deployment_location)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.generalization, OTIUMLA_generalization_specific)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.nestedNode, OTIUMLA_nestedNode_node)
    e9 <- 
      toCompositeFirstEndOrderedLinkExtent(e8, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e13 <- 
      toCompositeFirstEndOrderedLinkExtent(e12, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e20 <- 
      toCompositeLinkExtent(e19, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e21 <-
      toReferenceLinkExtent(e20, u, u.useCase, OTIUMLA_subject_useCase)
    result = e21  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLObjectFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLObjectFlow(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isMulticast = u.isMulticast,
            isMultireceive = u.isMultireceive,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.guard, OTIUMLA_guard_activityEdge)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.weight, OTIUMLA_weight_activityEdge)
    e5 <-
      toReferenceLinkExtent(e4, u, u.interrupts, OTIUMLA_interruptingEdge_interrupts)
    e6 <-
      toReferenceLinkExtent(e5, u, u.source, OTIUMLA_outgoing_source_node)
    e7 <-
      toReferenceLinkExtent(e6, u, u.target, OTIUMLA_incoming_target_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLOccurrenceSpecification(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLOpaqueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLOpaqueAction(
            toolSpecific_id = u.toolSpecific_id,
            body = u.body,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            language = u.language,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.inputValue, OTIUMLA_inputValue_opaqueAction)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.outputValue, OTIUMLA_outputValue_opaqueAction)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLOpaqueBehavior[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e11 <- 
      toCompositeFirstEndOrderedLinkExtent(e10, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e12 <- 
      toCompositeFirstEndOrderedLinkExtent(e11, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e20 <- 
      toCompositeLinkExtent(e19, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e21 <-
      toReferenceLinkExtent(e20, u, u.useCase, OTIUMLA_subject_useCase)
    result = e21  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLOpaqueExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLOpaqueExpression(
            toolSpecific_id = u.toolSpecific_id,
            body = u.body,
            language = u.language,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLOperation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLOperation(
            toolSpecific_id = u.toolSpecific_id,
            concurrency = u.concurrency,
            isAbstract = u.isAbstract,
            isLeaf = u.isLeaf,
            isQuery = u.isQuery,
            isStatic = u.isStatic,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeFirstEndOrderedLinkExtent(e3, u, u.ownedParameter, OTIUMLA_ownedParameter_operation)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavioralFeature)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e10 <-
      toReferenceLinkExtent(e9, u, u.method, OTIUMLA_method_specification)
    e11 <-
      toReferenceLinkExtent(e10, u, u.templateParameter, OTIUMLA_operation_templateParameter_parameteredElement)
    result = e11  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLOperationTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLOperationTemplateParameter(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLOutputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    e6 <-
      toReferenceLinkExtent(e5, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e6  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLPackage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLPackage(
            toolSpecific_id = u.toolSpecific_id,
            URI = u.URI,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.packageMerge, OTIUMLA_packageMerge_receivingPackage)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.packagedElement, OTIUMLA_packagedElement_owningPackage)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.profileApplication, OTIUMLA_profileApplication_applyingPackage)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    result = e10  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLPackageImport[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLPackageImport(
            toolSpecific_id = u.toolSpecific_id,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e1  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLPackageMerge[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLPackageMerge(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e1  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.defaultValue, OTIUMLA_defaultValue_owningParameter)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    e6 <-
      toReferenceLinkExtent(e5, u, u.parameterSet, OTIUMLA_parameterSet_parameter)
    e7 <-
      toReferenceLinkExtent(e6, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLParameterSet[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLParameterSet(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.condition, OTIUMLA_condition_parameterSet)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLPartDecomposition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLPartDecomposition(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.actualGate, OTIUMLA_actualGate_interactionUse)
    e2 <- 
      toCompositeFirstEndOrderedLinkExtent(e1, u, u.argument, OTIUMLA_argument_interactionUse)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.returnValue, OTIUMLA_returnValue_interactionUse)
    e7 <-
      toReferenceLinkExtent(e6, u, u.covered, OTIUMLA_covered_coveredBy)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLPort[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.defaultValue, OTIUMLA_defaultValue_owningProperty)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.deployment, OTIUMLA_deployment_location)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.qualifier, OTIUMLA_qualifier_associationEnd)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    e8 <-
      toReferenceSecondEndOrderedLinkExtent(e7, u, u.association, OTIUMLA_memberEnd_association)
    e9 <-
      toReferenceLinkExtent(e8, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement)
    result = e9  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLPrimitiveType[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLPrimitiveType(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeFirstEndOrderedLinkExtent(e4, u, u.ownedAttribute, OTIUMLA_ownedAttribute_datatype)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.ownedOperation, OTIUMLA_ownedOperation_datatype)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e14 <-
      toReferenceLinkExtent(e13, u, u.useCase, OTIUMLA_subject_useCase)
    result = e14  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLProfile[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLProfile(
            toolSpecific_id = u.toolSpecific_id,
            URI = u.URI,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.packageMerge, OTIUMLA_packageMerge_receivingPackage)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.packagedElement, OTIUMLA_packagedElement_owningPackage)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.profileApplication, OTIUMLA_profileApplication_applyingPackage)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    result = e10  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLProfileApplication[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLProfileApplication(
            toolSpecific_id = u.toolSpecific_id,
            isStrict = u.isStrict))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e1  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLProperty[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.defaultValue, OTIUMLA_defaultValue_owningProperty)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.deployment, OTIUMLA_deployment_location)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.qualifier, OTIUMLA_qualifier_associationEnd)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    e8 <-
      toReferenceSecondEndOrderedLinkExtent(e7, u, u.association, OTIUMLA_memberEnd_association)
    e9 <-
      toReferenceLinkExtent(e8, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement)
    result = e9  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLProtocolConformance[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLProtocolConformance(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e1  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLProtocolStateMachine[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLProtocolStateMachine(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            isReentrant = u.isReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.conformance, OTIUMLA_conformance_specificMachine)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.connectionPoint, OTIUMLA_connectionPoint_stateMachine)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.generalization, OTIUMLA_generalization_specific)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e8 <- 
      toCompositeFirstEndOrderedLinkExtent(e7, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e9 <- 
      toCompositeFirstEndOrderedLinkExtent(e8, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e13 <- 
      toCompositeFirstEndOrderedLinkExtent(e12, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e14 <- 
      toCompositeFirstEndOrderedLinkExtent(e13, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e20 <- 
      toCompositeLinkExtent(e19, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e21 <- 
      toCompositeLinkExtent(e20, u, u.region, OTIUMLA_region_stateMachine)
    e22 <- 
      toCompositeLinkExtent(e21, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e23 <- 
      toCompositeLinkExtent(e22, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e24 <-
      toReferenceLinkExtent(e23, u, u.useCase, OTIUMLA_subject_useCase)
    result = e24  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLProtocolTransition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLProtocolTransition(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            kind = u.kind,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.effect, OTIUMLA_effect_transition)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.preCondition, OTIUMLA_preCondition_protocolTransition)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.trigger, OTIUMLA_trigger_transition)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLPseudostate[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLPseudostate(
            toolSpecific_id = u.toolSpecific_id,
            kind = u.kind,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLQualifierValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLQualifierValue(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e1  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLRaiseExceptionAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLRaiseExceptionAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.exception, OTIUMLA_exception_raiseExceptionAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReadExtentAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReadExtentAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.result, OTIUMLA_result_readExtentAction)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReadIsClassifiedObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReadIsClassifiedObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isDirect = u.isDirect,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.object, OTIUMLA_object_readIsClassifiedObjectAction)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.result, OTIUMLA_result_readIsClassifiedObjectAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReadLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReadLinkAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.endData, OTIUMLA_endData_linkAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.inputValue, OTIUMLA_inputValue_linkAction)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.result, OTIUMLA_result_readLinkAction)
    e9 <-
      toReferenceLinkExtent(e8, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e9  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReadLinkObjectEndAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReadLinkObjectEndAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.object, OTIUMLA_object_readLinkObjectEndAction)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.result, OTIUMLA_result_readLinkObjectEndAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReadLinkObjectEndQualifierAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReadLinkObjectEndQualifierAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.object, OTIUMLA_object_readLinkObjectEndQualifierAction)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.result, OTIUMLA_result_readLinkObjectEndQualifierAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReadSelfAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReadSelfAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.result, OTIUMLA_result_readSelfAction)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReadStructuralFeatureAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReadStructuralFeatureAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.object, OTIUMLA_object_structuralFeatureAction)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.result, OTIUMLA_result_readStructuralFeatureAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReadVariableAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReadVariableAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.result, OTIUMLA_result_readVariableAction)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLRealization(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.mapping, OTIUMLA_mapping_abstraction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReception[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReception(
            toolSpecific_id = u.toolSpecific_id,
            concurrency = u.concurrency,
            isAbstract = u.isAbstract,
            isLeaf = u.isLeaf,
            isStatic = u.isStatic,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeFirstEndOrderedLinkExtent(e3, u, u.ownedParameter, OTIUMLA_ownedParameter_ownerFormalParam)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavioralFeature)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e8 <-
      toReferenceLinkExtent(e7, u, u.method, OTIUMLA_method_specification)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReclassifyObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReclassifyObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isReplaceAll = u.isReplaceAll,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.object, OTIUMLA_object_reclassifyObjectAction)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLRedefinableTemplateSignature[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLRedefinableTemplateSignature(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeFirstEndOrderedLinkExtent(e2, u, u.ownedParameter, OTIUMLA_ownedParameter_signature)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReduceAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReduceAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isOrdered = u.isOrdered,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collection, OTIUMLA_collection_reduceAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.result, OTIUMLA_result_reduceAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLRegion(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.subvertex, OTIUMLA_subvertex_container)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.transition, OTIUMLA_transition_container)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLRemoveStructuralFeatureValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLRemoveStructuralFeatureValueAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isRemoveDuplicates = u.isRemoveDuplicates,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.object, OTIUMLA_object_structuralFeatureAction)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.removeAt, OTIUMLA_removeAt_removeStructuralFeatureValueAction)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.result, OTIUMLA_result_writeStructuralFeatureAction)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.value, OTIUMLA_value_writeStructuralFeatureAction)
    e10 <-
      toReferenceLinkExtent(e9, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e10  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLRemoveVariableValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLRemoveVariableValueAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isRemoveDuplicates = u.isRemoveDuplicates,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.removeAt, OTIUMLA_removeAt_removeVariableValueAction)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.value, OTIUMLA_value_writeVariableAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLReplyAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLReplyAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeFirstEndOrderedLinkExtent(e5, u, u.replyValue, OTIUMLA_replyValue_replyAction)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.returnInformation, OTIUMLA_returnInformation_replyAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLSendObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLSendObjectAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.request, OTIUMLA_request_sendObjectAction)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.target, OTIUMLA_target_sendObjectAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLSendSignalAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLSendSignalAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeFirstEndOrderedLinkExtent(e0, u, u.argument, OTIUMLA_argument_invocationAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.target, OTIUMLA_target_sendSignalAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLSequenceNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLSequenceNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            mustIsolate = u.mustIsolate,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.edge, OTIUMLA_edge_inStructuredNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeFirstEndOrderedLinkExtent(e2, u, u.executableNode, OTIUMLA_executableNode_sequenceNode)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.handler, OTIUMLA_handler_protectedNode)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.structuredNodeOutput, OTIUMLA_structuredNodeOutput_structuredActivityNode)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.variable, OTIUMLA_variable_scope)
    e14 <-
      toReferenceLinkExtent(e13, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e14  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLSignal[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLSignal(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeFirstEndOrderedLinkExtent(e4, u, u.ownedAttribute, OTIUMLA_ownedAttribute_owningSignal)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e13 <-
      toReferenceLinkExtent(e12, u, u.useCase, OTIUMLA_subject_useCase)
    result = e13  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLSignalEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLSignalEvent(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLSlot[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLSlot(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <- 
      toCompositeFirstEndOrderedLinkExtent(e1, u, u.value, OTIUMLA_value_owningSlot)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLStartClassifierBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLStartClassifierBehaviorAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.object, OTIUMLA_object_startClassifierBehaviorAction)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLStartObjectBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLStartObjectBehaviorAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            isSynchronous = u.isSynchronous,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeFirstEndOrderedLinkExtent(e0, u, u.argument, OTIUMLA_argument_invocationAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.object, OTIUMLA_object_startObjectBehaviorAction)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e8 <- 
      toCompositeFirstEndOrderedLinkExtent(e7, u, u.result, OTIUMLA_result_callAction)
    e9 <-
      toReferenceLinkExtent(e8, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e9  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLState[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLState(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.connection, OTIUMLA_connection_state)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.connectionPoint, OTIUMLA_connectionPoint_state)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.deferrableTrigger, OTIUMLA_deferrableTrigger_state)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.doActivity, OTIUMLA_doActivity_state)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.entry, OTIUMLA_entry_state)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.exit, OTIUMLA_exit_state)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.region, OTIUMLA_region_state)
    e13 <-
      toReferenceLinkExtent(e12, u, u.submachine, OTIUMLA_submachineState_submachine)
    result = e13  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLStateInvariant[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLStateInvariant(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.generalOrdering, OTIUMLA_generalOrdering_interactionFragment)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.invariant, OTIUMLA_invariant_stateInvariant)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e4  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLStateMachine[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLStateMachine(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            isReentrant = u.isReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.connectionPoint, OTIUMLA_connectionPoint_stateMachine)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.generalization, OTIUMLA_generalization_specific)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e8 <- 
      toCompositeFirstEndOrderedLinkExtent(e7, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e12 <- 
      toCompositeFirstEndOrderedLinkExtent(e11, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e13 <- 
      toCompositeFirstEndOrderedLinkExtent(e12, u, u.ownedParameter, OTIUMLA_ownedParameter_behavior)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.ownedParameterSet, OTIUMLA_ownedParameterSet_behavior)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e20 <- 
      toCompositeLinkExtent(e19, u, u.region, OTIUMLA_region_stateMachine)
    e21 <- 
      toCompositeLinkExtent(e20, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e22 <- 
      toCompositeLinkExtent(e21, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e23 <-
      toReferenceLinkExtent(e22, u, u.useCase, OTIUMLA_subject_useCase)
    result = e23  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLStereotype[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLStereotype(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isActive = u.isActive,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.generalization, OTIUMLA_generalization_specific)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.icon, OTIUMLA_icon_stereotype)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.nestedClassifier, OTIUMLA_nestedClassifier_nestingClass)
    e8 <- 
      toCompositeFirstEndOrderedLinkExtent(e7, u, u.ownedAttribute, OTIUMLA_ownedAttribute_class)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedConnector, OTIUMLA_ownedConnector_structuredClassifier)
    e12 <- 
      toCompositeFirstEndOrderedLinkExtent(e11, u, u.ownedOperation, OTIUMLA_ownedOperation_class)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.ownedReception, OTIUMLA_ownedReception_class)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e17 <- 
      toCompositeLinkExtent(e16, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e18 <- 
      toCompositeLinkExtent(e17, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e19 <- 
      toCompositeLinkExtent(e18, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e20 <-
      toReferenceLinkExtent(e19, u, u.useCase, OTIUMLA_subject_useCase)
    result = e20  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLStringExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLStringExpression(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            symbol = u.symbol,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeFirstEndOrderedLinkExtent(e1, u, u.operand, OTIUMLA_operand_expression)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_template)
    e5 <- 
      toCompositeFirstEndOrderedLinkExtent(e4, u, u.subExpression, OTIUMLA_subExpression_owningExpression)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    result = e6  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLStructuredActivityNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLStructuredActivityNode(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            mustIsolate = u.mustIsolate,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.edge, OTIUMLA_edge_inStructuredNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.handler, OTIUMLA_handler_protectedNode)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.node, OTIUMLA_node_inStructuredNode)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.structuredNodeInput, OTIUMLA_structuredNodeInput_structuredActivityNode)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.structuredNodeOutput, OTIUMLA_structuredNodeOutput_structuredActivityNode)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.variable, OTIUMLA_variable_scope)
    e14 <-
      toReferenceLinkExtent(e13, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e14  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLSubstitution[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLSubstitution(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.mapping, OTIUMLA_mapping_abstraction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTemplateBinding[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTemplateBinding(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.parameterSubstitution, OTIUMLA_parameterSubstitution_templateBinding)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTemplateParameter(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedDefault, OTIUMLA_ownedDefault_templateParameter)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedParameteredElement, OTIUMLA_ownedParameteredElement_owningTemplateParameter)
    e4 <-
      toReferenceLinkExtent(e3, u, u.parameteredElement, OTIUMLA_parameteredElement_templateParameter)
    result = e4  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTemplateParameterSubstitution[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTemplateParameterSubstitution(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedActual, OTIUMLA_ownedActual_owningTemplateParameterSubstitution)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTemplateSignature[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTemplateSignature(
            toolSpecific_id = u.toolSpecific_id))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e2 <- 
      toCompositeFirstEndOrderedLinkExtent(e1, u, u.ownedParameter, OTIUMLA_ownedParameter_signature)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTestIdentityAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTestIdentityAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.first, OTIUMLA_first_testIdentityAction)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.handler, OTIUMLA_handler_protectedNode)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.result, OTIUMLA_result_testIdentityAction)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.second, OTIUMLA_second_testIdentityAction)
    e9 <-
      toReferenceLinkExtent(e8, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e9  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTimeConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTimeConstraint(
            toolSpecific_id = u.toolSpecific_id,
            firstEvent = u.firstEvent,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.specification, OTIUMLA_specification_timeConstraint)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTimeEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTimeEvent(
            toolSpecific_id = u.toolSpecific_id,
            isRelative = u.isRelative,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.when, OTIUMLA_when_timeEvent)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTimeExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTimeExpression(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.expr, OTIUMLA_expr_timeExpression)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e3  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTimeInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTimeInterval(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTimeObservation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTimeObservation(
            toolSpecific_id = u.toolSpecific_id,
            firstEvent = u.firstEvent,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTransition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTransition(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            kind = u.kind,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.effect, OTIUMLA_effect_transition)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.trigger, OTIUMLA_trigger_transition)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLTrigger[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLTrigger(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLUnmarshallAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLUnmarshallAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.object, OTIUMLA_object_unmarshallAction)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e7 <- 
      toCompositeFirstEndOrderedLinkExtent(e6, u, u.result, OTIUMLA_result_unmarshallAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLUsage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLUsage(
            toolSpecific_id = u.toolSpecific_id,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    result = e2  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLUseCase[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLUseCase(
            toolSpecific_id = u.toolSpecific_id,
            isAbstract = u.isAbstract,
            isFinalSpecialization = u.isFinalSpecialization,
            isLeaf = u.isLeaf,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.collaborationUse, OTIUMLA_collaborationUse_classifier)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.elementImport, OTIUMLA_elementImport_importingNamespace)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.extend, OTIUMLA_extend_extension)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.extensionPoint, OTIUMLA_extensionPoint_useCase)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.generalization, OTIUMLA_generalization_specific)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.include, OTIUMLA_include_includingCase)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.interfaceRealization, OTIUMLA_interfaceRealization_implementingClassifier)
    e8 <- 
      toCompositeLinkExtent(e7, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e9 <- 
      toCompositeLinkExtent(e8, u, u.ownedBehavior, OTIUMLA_ownedBehavior_behavioredClassifier)
    e10 <- 
      toCompositeLinkExtent(e9, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e11 <- 
      toCompositeLinkExtent(e10, u, u.ownedRule, OTIUMLA_ownedRule_context)
    e12 <- 
      toCompositeLinkExtent(e11, u, u.ownedTemplateSignature, OTIUMLA_ownedTemplateSignature_classifier)
    e13 <- 
      toCompositeLinkExtent(e12, u, u.ownedUseCase, OTIUMLA_ownedUseCase_classifier)
    e14 <- 
      toCompositeLinkExtent(e13, u, u.packageImport, OTIUMLA_packageImport_importingNamespace)
    e15 <- 
      toCompositeLinkExtent(e14, u, u.substitution, OTIUMLA_substitution_substitutingClassifier)
    e16 <- 
      toCompositeLinkExtent(e15, u, u.templateBinding, OTIUMLA_templateBinding_boundElement)
    e17 <-
      toReferenceLinkExtent(e16, u, u.useCase, OTIUMLA_subject_useCase)
    result = e17  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLValuePin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
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
    e1 <- 
      toCompositeLinkExtent(e0, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.upperBound, OTIUMLA_upperBound_objectNode)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.value, OTIUMLA_value_valuePin)
    e7 <-
      toReferenceLinkExtent(e6, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e7  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLValueSpecificationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLValueSpecificationAction(
            toolSpecific_id = u.toolSpecific_id,
            isLeaf = u.isLeaf,
            isLocallyReentrant = u.isLocallyReentrant,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.handler, OTIUMLA_handler_protectedNode)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.localPostcondition, OTIUMLA_localPostcondition_action)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.localPrecondition, OTIUMLA_localPrecondition_action)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e5 <- 
      toCompositeLinkExtent(e4, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e6 <- 
      toCompositeLinkExtent(e5, u, u.result, OTIUMLA_result_valueSpecificationAction)
    e7 <- 
      toCompositeLinkExtent(e6, u, u.value, OTIUMLA_value_valueSpecificationAction)
    e8 <-
      toReferenceLinkExtent(e7, u, u.inInterruptibleRegion, OTIUMLA_inInterruptibleRegion_node)
    result = e8  
  } yield result

  def toOTI
  (extent: OTIDocumentExtent,
   u : UMLVariable[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = for {
    e0 <- extent.copy(
      elementExtent = 
        extent.elementExtent +
          OTIMOFElement.OTIUMLVariable(
            toolSpecific_id = u.toolSpecific_id,
            isOrdered = u.isOrdered,
            isUnique = u.isUnique,
            name = u.name,
            visibility = u.visibility))
    e1 <- 
      toCompositeLinkExtent(e0, u, u.lowerValue, OTIUMLA_lowerValue_owningLower)
    e2 <- 
      toCompositeLinkExtent(e1, u, u.nameExpression, OTIUMLA_nameExpression_namedElement)
    e3 <- 
      toCompositeLinkExtent(e2, u, u.ownedComment, OTIUMLA_ownedComment_owningElement)
    e4 <- 
      toCompositeLinkExtent(e3, u, u.upperValue, OTIUMLA_upperValue_owningUpper)
    e5 <-
      toReferenceLinkExtent(e4, u, u.templateParameter, OTIUMLA_connectableElement_templateParameter_parameteredElement)
    result = e5  
  } yield result
