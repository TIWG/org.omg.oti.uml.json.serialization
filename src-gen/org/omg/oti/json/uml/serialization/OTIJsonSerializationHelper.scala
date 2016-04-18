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

import org.omg.oti.json.common.OTIPrimitiveTypes.TOOL_SPECIFIC_ID
import org.omg.oti.json.uml._

import scala.{Boolean,Double,Function1,Int,Option}
import scala.Predef.Integer2int
// <!-- End of user code imports -->

object OTIJsonSerializationHelper {

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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLAbstraction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLAbstraction(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLAcceptCallAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLAcceptCallAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        isUnmarshall = u.isUnmarshall,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLAcceptEventAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLAcceptEventAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        isUnmarshall = u.isUnmarshall,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLActionExecutionSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLActionExecutionSpecification(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLActionInputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLActivity[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLActivityFinalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLActivityFinalNode(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLActivityParameterNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLActivityParameterNode(
        toolSpecific_id = u.toolSpecific_id,
        isControlType = u.isControlType,
        isLeaf = u.isLeaf,
        name = u.name,
        ordering = u.ordering,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLActivityPartition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLActivityPartition(
        toolSpecific_id = u.toolSpecific_id,
        isDimension = u.isDimension,
        isExternal = u.isExternal,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLActor[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLActor(
        toolSpecific_id = u.toolSpecific_id,
        isAbstract = u.isAbstract,
        isFinalSpecialization = u.isFinalSpecialization,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLAddStructuralFeatureValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLAddStructuralFeatureValueAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        isReplaceAll = u.isReplaceAll,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLAddVariableValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLAddVariableValueAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        isReplaceAll = u.isReplaceAll,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLAnyReceiveEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLAnyReceiveEvent(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLArtifact[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLAssociation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLAssociationClass[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLBehaviorExecutionSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLBehaviorExecutionSpecification(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLBroadcastSignalAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLBroadcastSignalAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLCallBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLCallBehaviorAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        isSynchronous = u.isSynchronous,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLCallEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLCallEvent(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLCallOperationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLCallOperationAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        isSynchronous = u.isSynchronous,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLCentralBufferNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLCentralBufferNode(
        toolSpecific_id = u.toolSpecific_id,
        isControlType = u.isControlType,
        isLeaf = u.isLeaf,
        name = u.name,
        ordering = u.ordering,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLChangeEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLChangeEvent(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLClass[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLClassifierTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLClassifierTemplateParameter(
        toolSpecific_id = u.toolSpecific_id,
        allowSubstitutable = u.allowSubstitutable))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLClause[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLClause(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLClearAssociationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLClearAssociationAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLClearStructuralFeatureAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLClearStructuralFeatureAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLClearVariableAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLClearVariableAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLCollaboration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLCollaboration(
        toolSpecific_id = u.toolSpecific_id,
        isAbstract = u.isAbstract,
        isFinalSpecialization = u.isFinalSpecialization,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLCollaborationUse[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLCollaborationUse(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLCombinedFragment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLCombinedFragment(
        toolSpecific_id = u.toolSpecific_id,
        interactionOperator = u.interactionOperator,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLComment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLComment(
        toolSpecific_id = u.toolSpecific_id,
        body = u.body))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLCommunicationPath[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLComponent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLComponentRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLComponentRealization(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLConditionalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLConnectableElementTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLConnectableElementTemplateParameter(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLConnectionPointReference[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLConnectionPointReference(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLConnector[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLConnector(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isStatic = u.isStatic,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLConnectorEnd[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLConnectorEnd(
        toolSpecific_id = u.toolSpecific_id,
        isOrdered = u.isOrdered,
        isUnique = u.isUnique))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLConsiderIgnoreFragment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLConsiderIgnoreFragment(
        toolSpecific_id = u.toolSpecific_id,
        interactionOperator = u.interactionOperator,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLConstraint(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLContinuation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLContinuation(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        setting = u.setting,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLControlFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLControlFlow(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLCreateLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLCreateLinkAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLCreateLinkObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLCreateLinkObjectAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLCreateObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLCreateObjectAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDataStoreNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLDataStoreNode(
        toolSpecific_id = u.toolSpecific_id,
        isControlType = u.isControlType,
        isLeaf = u.isLeaf,
        name = u.name,
        ordering = u.ordering,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDataType[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLDataType(
        toolSpecific_id = u.toolSpecific_id,
        isAbstract = u.isAbstract,
        isFinalSpecialization = u.isFinalSpecialization,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDecisionNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLDecisionNode(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDependency[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLDependency(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDeployment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLDeployment(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDeploymentSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDestroyLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLDestroyLinkAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDestroyObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDestructionOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLDestructionOccurrenceSpecification(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDevice[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDuration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLDuration(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDurationConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLDurationConstraint(
        toolSpecific_id = u.toolSpecific_id,
        firstEvent = u.firstEvent,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDurationInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLDurationInterval(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLDurationObservation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLDurationObservation(
        toolSpecific_id = u.toolSpecific_id,
        firstEvent = u.firstEvent,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLElementImport[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLElementImport(
        toolSpecific_id = u.toolSpecific_id,
        alias = u.alias,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLEnumeration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLEnumeration(
        toolSpecific_id = u.toolSpecific_id,
        isAbstract = u.isAbstract,
        isFinalSpecialization = u.isFinalSpecialization,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLEnumerationLiteral[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLEnumerationLiteral(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLExceptionHandler[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLExceptionHandler(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLExecutionEnvironment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLExecutionOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLExecutionOccurrenceSpecification(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLExpansionNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLExpansionNode(
        toolSpecific_id = u.toolSpecific_id,
        isControlType = u.isControlType,
        isLeaf = u.isLeaf,
        name = u.name,
        ordering = u.ordering,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLExpansionRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLExpression(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        symbol = u.symbol,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLExtend[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLExtend(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLExtension[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLExtensionEnd[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLExtensionPoint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLExtensionPoint(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLFinalState[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLFinalState(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLFlowFinalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLFlowFinalNode(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLForkNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLForkNode(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLFunctionBehavior[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLGate[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLGate(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLGeneralOrdering[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLGeneralOrdering(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLGeneralization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLGeneralization(
        toolSpecific_id = u.toolSpecific_id,
        isSubstitutable = u.isSubstitutable))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLGeneralizationSet[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLGeneralizationSet(
        toolSpecific_id = u.toolSpecific_id,
        isCovering = u.isCovering,
        isDisjoint = u.isDisjoint,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLImage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLImage(
        toolSpecific_id = u.toolSpecific_id,
        content = u.content,
        format = u.format,
        location = u.location))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInclude[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInclude(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInformationFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInformationFlow(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInformationItem[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInformationItem(
        toolSpecific_id = u.toolSpecific_id,
        isAbstract = u.isAbstract,
        isFinalSpecialization = u.isFinalSpecialization,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInitialNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInitialNode(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInstanceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInstanceSpecification(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInstanceValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInstanceValue(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInteraction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInteractionConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInteractionConstraint(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInteractionOperand[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInteractionOperand(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInteractionUse[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInteractionUse(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInterface[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInterface(
        toolSpecific_id = u.toolSpecific_id,
        isAbstract = u.isAbstract,
        isFinalSpecialization = u.isFinalSpecialization,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInterfaceRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInterfaceRealization(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInterruptibleActivityRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInterruptibleActivityRegion(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLInterval(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLIntervalConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLIntervalConstraint(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLJoinNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLJoinNode(
        toolSpecific_id = u.toolSpecific_id,
        isCombineDuplicate = u.isCombineDuplicate,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLLifeline[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLLifeline(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLLinkEndCreationData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLLinkEndCreationData(
        toolSpecific_id = u.toolSpecific_id,
        isReplaceAll = u.isReplaceAll))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLLinkEndData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLLinkEndData(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLLinkEndDestructionData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLLinkEndDestructionData(
        toolSpecific_id = u.toolSpecific_id,
        isDestroyDuplicates = u.isDestroyDuplicates))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLLiteralBoolean[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLLiteralBoolean(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        value = u.value,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLLiteralInteger[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLLiteralInteger(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        value = u.value,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLLiteralNull[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLLiteralNull(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLLiteralReal[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLLiteralReal(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        value = u.value,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLLiteralString[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLLiteralString(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        value = u.value,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLLiteralUnlimitedNatural[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLLiteralUnlimitedNatural(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        value = u.value,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLLoopNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLManifestation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLManifestation(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLMergeNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLMergeNode(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLMessage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLMessage(
        toolSpecific_id = u.toolSpecific_id,
        messageSort = u.messageSort,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLMessageOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLMessageOccurrenceSpecification(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLModel[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLModel(
        toolSpecific_id = u.toolSpecific_id,
        URI = u.URI,
        name = u.name,
        viewpoint = u.viewpoint,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLObjectFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLObjectFlow(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isMulticast = u.isMulticast,
        isMultireceive = u.isMultireceive,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLOccurrenceSpecification(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLOpaqueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLOpaqueBehavior[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLOpaqueExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLOpaqueExpression(
        toolSpecific_id = u.toolSpecific_id,
        body = u.body,
        language = u.language,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLOperation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLOperationTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLOperationTemplateParameter(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLOutputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLPackage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLPackage(
        toolSpecific_id = u.toolSpecific_id,
        URI = u.URI,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLPackageImport[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLPackageImport(
        toolSpecific_id = u.toolSpecific_id,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLPackageMerge[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLPackageMerge(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLParameterSet[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLParameterSet(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLPartDecomposition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLPartDecomposition(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLPort[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLPrimitiveType[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLPrimitiveType(
        toolSpecific_id = u.toolSpecific_id,
        isAbstract = u.isAbstract,
        isFinalSpecialization = u.isFinalSpecialization,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLProfile[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLProfile(
        toolSpecific_id = u.toolSpecific_id,
        URI = u.URI,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLProfileApplication[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLProfileApplication(
        toolSpecific_id = u.toolSpecific_id,
        isStrict = u.isStrict))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLProperty[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLProtocolConformance[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLProtocolConformance(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLProtocolStateMachine[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLProtocolTransition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLProtocolTransition(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        kind = u.kind,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLPseudostate[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLPseudostate(
        toolSpecific_id = u.toolSpecific_id,
        kind = u.kind,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLQualifierValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLQualifierValue(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLRaiseExceptionAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLRaiseExceptionAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReadExtentAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLReadExtentAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReadIsClassifiedObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLReadIsClassifiedObjectAction(
        toolSpecific_id = u.toolSpecific_id,
        isDirect = u.isDirect,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReadLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLReadLinkAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReadLinkObjectEndAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLReadLinkObjectEndAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReadLinkObjectEndQualifierAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLReadLinkObjectEndQualifierAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReadSelfAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLReadSelfAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReadStructuralFeatureAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLReadStructuralFeatureAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReadVariableAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLReadVariableAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLRealization(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReception[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReclassifyObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLReclassifyObjectAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        isReplaceAll = u.isReplaceAll,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLRedefinableTemplateSignature[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLRedefinableTemplateSignature(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReduceAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLReduceAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        isOrdered = u.isOrdered,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLRegion(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLRemoveStructuralFeatureValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLRemoveStructuralFeatureValueAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        isRemoveDuplicates = u.isRemoveDuplicates,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLRemoveVariableValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLRemoveVariableValueAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        isRemoveDuplicates = u.isRemoveDuplicates,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLReplyAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLReplyAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLSendObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLSendObjectAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLSendSignalAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLSendSignalAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLSequenceNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLSequenceNode(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        mustIsolate = u.mustIsolate,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLSignal[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLSignal(
        toolSpecific_id = u.toolSpecific_id,
        isAbstract = u.isAbstract,
        isFinalSpecialization = u.isFinalSpecialization,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLSignalEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLSignalEvent(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLSlot[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLSlot(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLStartClassifierBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLStartClassifierBehaviorAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLStartObjectBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLStartObjectBehaviorAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        isSynchronous = u.isSynchronous,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLState[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLState(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLStateInvariant[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLStateInvariant(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLStateMachine[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLStereotype[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLStringExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLStringExpression(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        symbol = u.symbol,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLStructuredActivityNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLStructuredActivityNode(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        mustIsolate = u.mustIsolate,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLSubstitution[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLSubstitution(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTemplateBinding[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTemplateBinding(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTemplateParameter(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTemplateParameterSubstitution[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTemplateParameterSubstitution(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTemplateSignature[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTemplateSignature(
        toolSpecific_id = u.toolSpecific_id))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTestIdentityAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTestIdentityAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTimeConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTimeConstraint(
        toolSpecific_id = u.toolSpecific_id,
        firstEvent = u.firstEvent,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTimeEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTimeEvent(
        toolSpecific_id = u.toolSpecific_id,
        isRelative = u.isRelative,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTimeExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTimeExpression(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTimeInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTimeInterval(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTimeObservation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTimeObservation(
        toolSpecific_id = u.toolSpecific_id,
        firstEvent = u.firstEvent,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTransition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTransition(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        kind = u.kind,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLTrigger[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLTrigger(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLUnmarshallAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLUnmarshallAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLUsage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLUsage(
        toolSpecific_id = u.toolSpecific_id,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLUseCase[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLUseCase(
        toolSpecific_id = u.toolSpecific_id,
        isAbstract = u.isAbstract,
        isFinalSpecialization = u.isFinalSpecialization,
        isLeaf = u.isLeaf,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLValuePin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
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

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLValueSpecificationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLValueSpecificationAction(
        toolSpecific_id = u.toolSpecific_id,
        isLeaf = u.isLeaf,
        isLocallyReentrant = u.isLocallyReentrant,
        name = u.name,
        visibility = u.visibility))

  def toOTI[Uml <: UML]
  (extent: OTIDocumentExtent,
   u : UMLVariable[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIDocumentExtent
  = extent.copy(
    elementExtent = 
      extent.elementExtent +
      OTIMOFElement.OTIUMLVariable(
        toolSpecific_id = u.toolSpecific_id,
        isOrdered = u.isOrdered,
        isUnique = u.isUnique,
        name = u.name,
        visibility = u.visibility))
}
