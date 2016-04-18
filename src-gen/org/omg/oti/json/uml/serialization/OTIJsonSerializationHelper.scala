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
import org.omg.oti.json.uml.enums._

import scala.collection.immutable._
import scala.{Boolean,Double,Int,Option}
import scala.Predef.String
import scalaz.@@
// <!-- End of user code imports -->

object OTIJsonSerializationHelper {

  def toOTI[Uml <: UML]
  (u : UMLAbstraction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLAbstraction
  = OTIMOFElement.OTIUMLAbstraction(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLAcceptCallAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLAcceptCallAction
  = OTIMOFElement.OTIUMLAcceptCallAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isUnmarshall = u.isUnmarshall,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLAcceptEventAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLAcceptEventAction
  = OTIMOFElement.OTIUMLAcceptEventAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isUnmarshall = u.isUnmarshall,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLActionExecutionSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLActionExecutionSpecification
  = OTIMOFElement.OTIUMLActionExecutionSpecification(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLActionInputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLActionInputPin
  = OTIMOFElement.OTIUMLActionInputPin(
    toolSpecific_id = u.toolSpecific_id,
    isControl = u.isControl,
    isControlType = u.isControlType,
    isLeaf = u.isLeaf,
    isOrdered = u.isOrdered,
    isUnique = u.isUnique,
    name = u.name,
    ordering = u.ordering,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLActivity[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLActivity
  = OTIMOFElement.OTIUMLActivity(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    isReadOnly = u.isReadOnly,
    isReentrant = u.isReentrant,
    isSingleExecution = u.isSingleExecution,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLActivityFinalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLActivityFinalNode
  = OTIMOFElement.OTIUMLActivityFinalNode(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLActivityParameterNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLActivityParameterNode
  = OTIMOFElement.OTIUMLActivityParameterNode(
    toolSpecific_id = u.toolSpecific_id,
    isControlType = u.isControlType,
    isLeaf = u.isLeaf,
    name = u.name,
    ordering = u.ordering,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLActivityPartition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLActivityPartition
  = OTIMOFElement.OTIUMLActivityPartition(
    toolSpecific_id = u.toolSpecific_id,
    isDimension = u.isDimension,
    isExternal = u.isExternal,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLActor[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLActor
  = OTIMOFElement.OTIUMLActor(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLAddStructuralFeatureValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLAddStructuralFeatureValueAction
  = OTIMOFElement.OTIUMLAddStructuralFeatureValueAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isReplaceAll = u.isReplaceAll,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLAddVariableValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLAddVariableValueAction
  = OTIMOFElement.OTIUMLAddVariableValueAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isReplaceAll = u.isReplaceAll,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLAnyReceiveEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLAnyReceiveEvent
  = OTIMOFElement.OTIUMLAnyReceiveEvent(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLArtifact[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLArtifact
  = OTIMOFElement.OTIUMLArtifact(
    toolSpecific_id = u.toolSpecific_id,
    fileName = u.fileName,
    isAbstract = u.isAbstract,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLAssociation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLAssociation
  = OTIMOFElement.OTIUMLAssociation(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isDerived = u.isDerived,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLAssociationClass[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLAssociationClass
  = OTIMOFElement.OTIUMLAssociationClass(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isDerived = u.isDerived,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLBehaviorExecutionSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLBehaviorExecutionSpecification
  = OTIMOFElement.OTIUMLBehaviorExecutionSpecification(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLBroadcastSignalAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLBroadcastSignalAction
  = OTIMOFElement.OTIUMLBroadcastSignalAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLCallBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLCallBehaviorAction
  = OTIMOFElement.OTIUMLCallBehaviorAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isSynchronous = u.isSynchronous,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLCallEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLCallEvent
  = OTIMOFElement.OTIUMLCallEvent(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLCallOperationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLCallOperationAction
  = OTIMOFElement.OTIUMLCallOperationAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isSynchronous = u.isSynchronous,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLCentralBufferNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLCentralBufferNode
  = OTIMOFElement.OTIUMLCentralBufferNode(
    toolSpecific_id = u.toolSpecific_id,
    isControlType = u.isControlType,
    isLeaf = u.isLeaf,
    name = u.name,
    ordering = u.ordering,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLChangeEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLChangeEvent
  = OTIMOFElement.OTIUMLChangeEvent(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLClass[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLClass
  = OTIMOFElement.OTIUMLClass(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLClassifierTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLClassifierTemplateParameter
  = OTIMOFElement.OTIUMLClassifierTemplateParameter(
    toolSpecific_id = u.toolSpecific_id,
    allowSubstitutable = u.allowSubstitutable)

  def toOTI[Uml <: UML]
  (u : UMLClause[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLClause
  = OTIMOFElement.OTIUMLClause(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLClearAssociationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLClearAssociationAction
  = OTIMOFElement.OTIUMLClearAssociationAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLClearStructuralFeatureAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLClearStructuralFeatureAction
  = OTIMOFElement.OTIUMLClearStructuralFeatureAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLClearVariableAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLClearVariableAction
  = OTIMOFElement.OTIUMLClearVariableAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLCollaboration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLCollaboration
  = OTIMOFElement.OTIUMLCollaboration(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLCollaborationUse[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLCollaborationUse
  = OTIMOFElement.OTIUMLCollaborationUse(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLCombinedFragment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLCombinedFragment
  = OTIMOFElement.OTIUMLCombinedFragment(
    toolSpecific_id = u.toolSpecific_id,
    interactionOperator = u.interactionOperator,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLComment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLComment
  = OTIMOFElement.OTIUMLComment(
    toolSpecific_id = u.toolSpecific_id,
    body = u.body)

  def toOTI[Uml <: UML]
  (u : UMLCommunicationPath[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLCommunicationPath
  = OTIMOFElement.OTIUMLCommunicationPath(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isDerived = u.isDerived,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLComponent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLComponent
  = OTIMOFElement.OTIUMLComponent(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isIndirectlyInstantiated = u.isIndirectlyInstantiated,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLComponentRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLComponentRealization
  = OTIMOFElement.OTIUMLComponentRealization(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLConditionalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLConditionalNode
  = OTIMOFElement.OTIUMLConditionalNode(
    toolSpecific_id = u.toolSpecific_id,
    isAssured = u.isAssured,
    isDeterminate = u.isDeterminate,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    mustIsolate = u.mustIsolate,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLConnectableElementTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLConnectableElementTemplateParameter
  = OTIMOFElement.OTIUMLConnectableElementTemplateParameter(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLConnectionPointReference[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLConnectionPointReference
  = OTIMOFElement.OTIUMLConnectionPointReference(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLConnector[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLConnector
  = OTIMOFElement.OTIUMLConnector(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isStatic = u.isStatic,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLConnectorEnd[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLConnectorEnd
  = OTIMOFElement.OTIUMLConnectorEnd(
    toolSpecific_id = u.toolSpecific_id,
    isOrdered = u.isOrdered,
    isUnique = u.isUnique)

  def toOTI[Uml <: UML]
  (u : UMLConsiderIgnoreFragment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLConsiderIgnoreFragment
  = OTIMOFElement.OTIUMLConsiderIgnoreFragment(
    toolSpecific_id = u.toolSpecific_id,
    interactionOperator = u.interactionOperator,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLConstraint
  = OTIMOFElement.OTIUMLConstraint(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLContinuation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLContinuation
  = OTIMOFElement.OTIUMLContinuation(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    setting = u.setting,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLControlFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLControlFlow
  = OTIMOFElement.OTIUMLControlFlow(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLCreateLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLCreateLinkAction
  = OTIMOFElement.OTIUMLCreateLinkAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLCreateLinkObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLCreateLinkObjectAction
  = OTIMOFElement.OTIUMLCreateLinkObjectAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLCreateObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLCreateObjectAction
  = OTIMOFElement.OTIUMLCreateObjectAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDataStoreNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDataStoreNode
  = OTIMOFElement.OTIUMLDataStoreNode(
    toolSpecific_id = u.toolSpecific_id,
    isControlType = u.isControlType,
    isLeaf = u.isLeaf,
    name = u.name,
    ordering = u.ordering,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDataType[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDataType
  = OTIMOFElement.OTIUMLDataType(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDecisionNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDecisionNode
  = OTIMOFElement.OTIUMLDecisionNode(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDependency[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDependency
  = OTIMOFElement.OTIUMLDependency(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDeployment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDeployment
  = OTIMOFElement.OTIUMLDeployment(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDeploymentSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDeploymentSpecification
  = OTIMOFElement.OTIUMLDeploymentSpecification(
    toolSpecific_id = u.toolSpecific_id,
    deploymentLocation = u.deploymentLocation,
    executionLocation = u.executionLocation,
    fileName = u.fileName,
    isAbstract = u.isAbstract,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDestroyLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDestroyLinkAction
  = OTIMOFElement.OTIUMLDestroyLinkAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDestroyObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDestroyObjectAction
  = OTIMOFElement.OTIUMLDestroyObjectAction(
    toolSpecific_id = u.toolSpecific_id,
    isDestroyLinks = u.isDestroyLinks,
    isDestroyOwnedObjects = u.isDestroyOwnedObjects,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDestructionOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDestructionOccurrenceSpecification
  = OTIMOFElement.OTIUMLDestructionOccurrenceSpecification(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDevice[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDevice
  = OTIMOFElement.OTIUMLDevice(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDuration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDuration
  = OTIMOFElement.OTIUMLDuration(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDurationConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDurationConstraint
  = OTIMOFElement.OTIUMLDurationConstraint(
    toolSpecific_id = u.toolSpecific_id,
    firstEvent = u.firstEvent,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDurationInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDurationInterval
  = OTIMOFElement.OTIUMLDurationInterval(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLDurationObservation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLDurationObservation
  = OTIMOFElement.OTIUMLDurationObservation(
    toolSpecific_id = u.toolSpecific_id,
    firstEvent = u.firstEvent,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLElementImport[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLElementImport
  = OTIMOFElement.OTIUMLElementImport(
    toolSpecific_id = u.toolSpecific_id,
    alias = u.alias,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLEnumeration[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLEnumeration
  = OTIMOFElement.OTIUMLEnumeration(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLEnumerationLiteral[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLEnumerationLiteral
  = OTIMOFElement.OTIUMLEnumerationLiteral(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLExceptionHandler[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLExceptionHandler
  = OTIMOFElement.OTIUMLExceptionHandler(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLExecutionEnvironment[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLExecutionEnvironment
  = OTIMOFElement.OTIUMLExecutionEnvironment(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLExecutionOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLExecutionOccurrenceSpecification
  = OTIMOFElement.OTIUMLExecutionOccurrenceSpecification(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLExpansionNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLExpansionNode
  = OTIMOFElement.OTIUMLExpansionNode(
    toolSpecific_id = u.toolSpecific_id,
    isControlType = u.isControlType,
    isLeaf = u.isLeaf,
    name = u.name,
    ordering = u.ordering,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLExpansionRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLExpansionRegion
  = OTIMOFElement.OTIUMLExpansionRegion(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    mode = u.mode,
    mustIsolate = u.mustIsolate,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLExpression
  = OTIMOFElement.OTIUMLExpression(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    symbol = u.symbol,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLExtend[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLExtend
  = OTIMOFElement.OTIUMLExtend(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLExtension[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLExtension
  = OTIMOFElement.OTIUMLExtension(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isDerived = u.isDerived,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLExtensionEnd[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLExtensionEnd
  = OTIMOFElement.OTIUMLExtensionEnd(
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
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLExtensionPoint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLExtensionPoint
  = OTIMOFElement.OTIUMLExtensionPoint(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLFinalState[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLFinalState
  = OTIMOFElement.OTIUMLFinalState(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLFlowFinalNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLFlowFinalNode
  = OTIMOFElement.OTIUMLFlowFinalNode(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLForkNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLForkNode
  = OTIMOFElement.OTIUMLForkNode(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLFunctionBehavior[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLFunctionBehavior
  = OTIMOFElement.OTIUMLFunctionBehavior(
    toolSpecific_id = u.toolSpecific_id,
    body = u.body,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    isReentrant = u.isReentrant,
    language = u.language,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLGate[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLGate
  = OTIMOFElement.OTIUMLGate(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLGeneralOrdering[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLGeneralOrdering
  = OTIMOFElement.OTIUMLGeneralOrdering(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLGeneralization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLGeneralization
  = OTIMOFElement.OTIUMLGeneralization(
    toolSpecific_id = u.toolSpecific_id,
    isSubstitutable = u.isSubstitutable)

  def toOTI[Uml <: UML]
  (u : UMLGeneralizationSet[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLGeneralizationSet
  = OTIMOFElement.OTIUMLGeneralizationSet(
    toolSpecific_id = u.toolSpecific_id,
    isCovering = u.isCovering,
    isDisjoint = u.isDisjoint,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLImage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLImage
  = OTIMOFElement.OTIUMLImage(
    toolSpecific_id = u.toolSpecific_id,
    content = u.content,
    format = u.format,
    location = u.location)

  def toOTI[Uml <: UML]
  (u : UMLInclude[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInclude
  = OTIMOFElement.OTIUMLInclude(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInformationFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInformationFlow
  = OTIMOFElement.OTIUMLInformationFlow(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInformationItem[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInformationItem
  = OTIMOFElement.OTIUMLInformationItem(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInitialNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInitialNode
  = OTIMOFElement.OTIUMLInitialNode(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInputPin
  = OTIMOFElement.OTIUMLInputPin(
    toolSpecific_id = u.toolSpecific_id,
    isControl = u.isControl,
    isControlType = u.isControlType,
    isLeaf = u.isLeaf,
    isOrdered = u.isOrdered,
    isUnique = u.isUnique,
    name = u.name,
    ordering = u.ordering,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInstanceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInstanceSpecification
  = OTIMOFElement.OTIUMLInstanceSpecification(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInstanceValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInstanceValue
  = OTIMOFElement.OTIUMLInstanceValue(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInteraction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInteraction
  = OTIMOFElement.OTIUMLInteraction(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    isReentrant = u.isReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInteractionConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInteractionConstraint
  = OTIMOFElement.OTIUMLInteractionConstraint(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInteractionOperand[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInteractionOperand
  = OTIMOFElement.OTIUMLInteractionOperand(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInteractionUse[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInteractionUse
  = OTIMOFElement.OTIUMLInteractionUse(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInterface[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInterface
  = OTIMOFElement.OTIUMLInterface(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInterfaceRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInterfaceRealization
  = OTIMOFElement.OTIUMLInterfaceRealization(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInterruptibleActivityRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInterruptibleActivityRegion
  = OTIMOFElement.OTIUMLInterruptibleActivityRegion(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLInterval
  = OTIMOFElement.OTIUMLInterval(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLIntervalConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLIntervalConstraint
  = OTIMOFElement.OTIUMLIntervalConstraint(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLJoinNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLJoinNode
  = OTIMOFElement.OTIUMLJoinNode(
    toolSpecific_id = u.toolSpecific_id,
    isCombineDuplicate = u.isCombineDuplicate,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLLifeline[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLLifeline
  = OTIMOFElement.OTIUMLLifeline(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLLinkEndCreationData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLLinkEndCreationData
  = OTIMOFElement.OTIUMLLinkEndCreationData(
    toolSpecific_id = u.toolSpecific_id,
    isReplaceAll = u.isReplaceAll)

  def toOTI[Uml <: UML]
  (u : UMLLinkEndData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLLinkEndData
  = OTIMOFElement.OTIUMLLinkEndData(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLLinkEndDestructionData[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLLinkEndDestructionData
  = OTIMOFElement.OTIUMLLinkEndDestructionData(
    toolSpecific_id = u.toolSpecific_id,
    isDestroyDuplicates = u.isDestroyDuplicates)

  def toOTI[Uml <: UML]
  (u : UMLLiteralBoolean[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLLiteralBoolean
  = OTIMOFElement.OTIUMLLiteralBoolean(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    value = u.value,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLLiteralInteger[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLLiteralInteger
  = OTIMOFElement.OTIUMLLiteralInteger(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    value = u.value,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLLiteralNull[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLLiteralNull
  = OTIMOFElement.OTIUMLLiteralNull(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLLiteralReal[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLLiteralReal
  = OTIMOFElement.OTIUMLLiteralReal(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    value = u.value,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLLiteralString[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLLiteralString
  = OTIMOFElement.OTIUMLLiteralString(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    value = u.value,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLLiteralUnlimitedNatural[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLLiteralUnlimitedNatural
  = OTIMOFElement.OTIUMLLiteralUnlimitedNatural(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    value = u.value,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLLoopNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLLoopNode
  = OTIMOFElement.OTIUMLLoopNode(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isTestedFirst = u.isTestedFirst,
    mustIsolate = u.mustIsolate,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLManifestation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLManifestation
  = OTIMOFElement.OTIUMLManifestation(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLMergeNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLMergeNode
  = OTIMOFElement.OTIUMLMergeNode(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLMessage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLMessage
  = OTIMOFElement.OTIUMLMessage(
    toolSpecific_id = u.toolSpecific_id,
    messageSort = u.messageSort,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLMessageOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLMessageOccurrenceSpecification
  = OTIMOFElement.OTIUMLMessageOccurrenceSpecification(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLModel[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLModel
  = OTIMOFElement.OTIUMLModel(
    toolSpecific_id = u.toolSpecific_id,
    URI = u.URI,
    name = u.name,
    viewpoint = u.viewpoint,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLNode
  = OTIMOFElement.OTIUMLNode(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLObjectFlow[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLObjectFlow
  = OTIMOFElement.OTIUMLObjectFlow(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isMulticast = u.isMulticast,
    isMultireceive = u.isMultireceive,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLOccurrenceSpecification[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLOccurrenceSpecification
  = OTIMOFElement.OTIUMLOccurrenceSpecification(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLOpaqueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLOpaqueAction
  = OTIMOFElement.OTIUMLOpaqueAction(
    toolSpecific_id = u.toolSpecific_id,
    body = u.body,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    language = u.language,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLOpaqueBehavior[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLOpaqueBehavior
  = OTIMOFElement.OTIUMLOpaqueBehavior(
    toolSpecific_id = u.toolSpecific_id,
    body = u.body,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    isReentrant = u.isReentrant,
    language = u.language,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLOpaqueExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLOpaqueExpression
  = OTIMOFElement.OTIUMLOpaqueExpression(
    toolSpecific_id = u.toolSpecific_id,
    body = u.body,
    language = u.language,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLOperation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLOperation
  = OTIMOFElement.OTIUMLOperation(
    toolSpecific_id = u.toolSpecific_id,
    concurrency = u.concurrency,
    isAbstract = u.isAbstract,
    isLeaf = u.isLeaf,
    isQuery = u.isQuery,
    isStatic = u.isStatic,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLOperationTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLOperationTemplateParameter
  = OTIMOFElement.OTIUMLOperationTemplateParameter(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLOutputPin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLOutputPin
  = OTIMOFElement.OTIUMLOutputPin(
    toolSpecific_id = u.toolSpecific_id,
    isControl = u.isControl,
    isControlType = u.isControlType,
    isLeaf = u.isLeaf,
    isOrdered = u.isOrdered,
    isUnique = u.isUnique,
    name = u.name,
    ordering = u.ordering,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLPackage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLPackage
  = OTIMOFElement.OTIUMLPackage(
    toolSpecific_id = u.toolSpecific_id,
    URI = u.URI,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLPackageImport[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLPackageImport
  = OTIMOFElement.OTIUMLPackageImport(
    toolSpecific_id = u.toolSpecific_id,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLPackageMerge[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLPackageMerge
  = OTIMOFElement.OTIUMLPackageMerge(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLParameter
  = OTIMOFElement.OTIUMLParameter(
    toolSpecific_id = u.toolSpecific_id,
    direction = u.direction,
    effect = u.effect,
    isException = u.isException,
    isOrdered = u.isOrdered,
    isStream = u.isStream,
    isUnique = u.isUnique,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLParameterSet[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLParameterSet
  = OTIMOFElement.OTIUMLParameterSet(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLPartDecomposition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLPartDecomposition
  = OTIMOFElement.OTIUMLPartDecomposition(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLPort[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLPort
  = OTIMOFElement.OTIUMLPort(
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
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLPrimitiveType[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLPrimitiveType
  = OTIMOFElement.OTIUMLPrimitiveType(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLProfile[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLProfile
  = OTIMOFElement.OTIUMLProfile(
    toolSpecific_id = u.toolSpecific_id,
    URI = u.URI,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLProfileApplication[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLProfileApplication
  = OTIMOFElement.OTIUMLProfileApplication(
    toolSpecific_id = u.toolSpecific_id,
    isStrict = u.isStrict)

  def toOTI[Uml <: UML]
  (u : UMLProperty[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLProperty
  = OTIMOFElement.OTIUMLProperty(
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
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLProtocolConformance[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLProtocolConformance
  = OTIMOFElement.OTIUMLProtocolConformance(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLProtocolStateMachine[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLProtocolStateMachine
  = OTIMOFElement.OTIUMLProtocolStateMachine(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    isReentrant = u.isReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLProtocolTransition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLProtocolTransition
  = OTIMOFElement.OTIUMLProtocolTransition(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    kind = u.kind,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLPseudostate[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLPseudostate
  = OTIMOFElement.OTIUMLPseudostate(
    toolSpecific_id = u.toolSpecific_id,
    kind = u.kind,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLQualifierValue[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLQualifierValue
  = OTIMOFElement.OTIUMLQualifierValue(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLRaiseExceptionAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLRaiseExceptionAction
  = OTIMOFElement.OTIUMLRaiseExceptionAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReadExtentAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReadExtentAction
  = OTIMOFElement.OTIUMLReadExtentAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReadIsClassifiedObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReadIsClassifiedObjectAction
  = OTIMOFElement.OTIUMLReadIsClassifiedObjectAction(
    toolSpecific_id = u.toolSpecific_id,
    isDirect = u.isDirect,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReadLinkAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReadLinkAction
  = OTIMOFElement.OTIUMLReadLinkAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReadLinkObjectEndAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReadLinkObjectEndAction
  = OTIMOFElement.OTIUMLReadLinkObjectEndAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReadLinkObjectEndQualifierAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReadLinkObjectEndQualifierAction
  = OTIMOFElement.OTIUMLReadLinkObjectEndQualifierAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReadSelfAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReadSelfAction
  = OTIMOFElement.OTIUMLReadSelfAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReadStructuralFeatureAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReadStructuralFeatureAction
  = OTIMOFElement.OTIUMLReadStructuralFeatureAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReadVariableAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReadVariableAction
  = OTIMOFElement.OTIUMLReadVariableAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLRealization[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLRealization
  = OTIMOFElement.OTIUMLRealization(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReception[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReception
  = OTIMOFElement.OTIUMLReception(
    toolSpecific_id = u.toolSpecific_id,
    concurrency = u.concurrency,
    isAbstract = u.isAbstract,
    isLeaf = u.isLeaf,
    isStatic = u.isStatic,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReclassifyObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReclassifyObjectAction
  = OTIMOFElement.OTIUMLReclassifyObjectAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isReplaceAll = u.isReplaceAll,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLRedefinableTemplateSignature[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLRedefinableTemplateSignature
  = OTIMOFElement.OTIUMLRedefinableTemplateSignature(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReduceAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReduceAction
  = OTIMOFElement.OTIUMLReduceAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isOrdered = u.isOrdered,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLRegion[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLRegion
  = OTIMOFElement.OTIUMLRegion(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLRemoveStructuralFeatureValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLRemoveStructuralFeatureValueAction
  = OTIMOFElement.OTIUMLRemoveStructuralFeatureValueAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isRemoveDuplicates = u.isRemoveDuplicates,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLRemoveVariableValueAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLRemoveVariableValueAction
  = OTIMOFElement.OTIUMLRemoveVariableValueAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isRemoveDuplicates = u.isRemoveDuplicates,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLReplyAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLReplyAction
  = OTIMOFElement.OTIUMLReplyAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLSendObjectAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLSendObjectAction
  = OTIMOFElement.OTIUMLSendObjectAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLSendSignalAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLSendSignalAction
  = OTIMOFElement.OTIUMLSendSignalAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLSequenceNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLSequenceNode
  = OTIMOFElement.OTIUMLSequenceNode(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    mustIsolate = u.mustIsolate,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLSignal[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLSignal
  = OTIMOFElement.OTIUMLSignal(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLSignalEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLSignalEvent
  = OTIMOFElement.OTIUMLSignalEvent(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLSlot[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLSlot
  = OTIMOFElement.OTIUMLSlot(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLStartClassifierBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLStartClassifierBehaviorAction
  = OTIMOFElement.OTIUMLStartClassifierBehaviorAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLStartObjectBehaviorAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLStartObjectBehaviorAction
  = OTIMOFElement.OTIUMLStartObjectBehaviorAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    isSynchronous = u.isSynchronous,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLState[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLState
  = OTIMOFElement.OTIUMLState(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLStateInvariant[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLStateInvariant
  = OTIMOFElement.OTIUMLStateInvariant(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLStateMachine[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLStateMachine
  = OTIMOFElement.OTIUMLStateMachine(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    isReentrant = u.isReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLStereotype[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLStereotype
  = OTIMOFElement.OTIUMLStereotype(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isActive = u.isActive,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLStringExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLStringExpression
  = OTIMOFElement.OTIUMLStringExpression(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    symbol = u.symbol,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLStructuredActivityNode[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLStructuredActivityNode
  = OTIMOFElement.OTIUMLStructuredActivityNode(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    mustIsolate = u.mustIsolate,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLSubstitution[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLSubstitution
  = OTIMOFElement.OTIUMLSubstitution(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLTemplateBinding[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTemplateBinding
  = OTIMOFElement.OTIUMLTemplateBinding(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLTemplateParameter[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTemplateParameter
  = OTIMOFElement.OTIUMLTemplateParameter(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLTemplateParameterSubstitution[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTemplateParameterSubstitution
  = OTIMOFElement.OTIUMLTemplateParameterSubstitution(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLTemplateSignature[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTemplateSignature
  = OTIMOFElement.OTIUMLTemplateSignature(
    toolSpecific_id = u.toolSpecific_id)

  def toOTI[Uml <: UML]
  (u : UMLTestIdentityAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTestIdentityAction
  = OTIMOFElement.OTIUMLTestIdentityAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLTimeConstraint[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTimeConstraint
  = OTIMOFElement.OTIUMLTimeConstraint(
    toolSpecific_id = u.toolSpecific_id,
    firstEvent = u.firstEvent,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLTimeEvent[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTimeEvent
  = OTIMOFElement.OTIUMLTimeEvent(
    toolSpecific_id = u.toolSpecific_id,
    isRelative = u.isRelative,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLTimeExpression[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTimeExpression
  = OTIMOFElement.OTIUMLTimeExpression(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLTimeInterval[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTimeInterval
  = OTIMOFElement.OTIUMLTimeInterval(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLTimeObservation[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTimeObservation
  = OTIMOFElement.OTIUMLTimeObservation(
    toolSpecific_id = u.toolSpecific_id,
    firstEvent = u.firstEvent,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLTransition[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTransition
  = OTIMOFElement.OTIUMLTransition(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    kind = u.kind,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLTrigger[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLTrigger
  = OTIMOFElement.OTIUMLTrigger(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLUnmarshallAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLUnmarshallAction
  = OTIMOFElement.OTIUMLUnmarshallAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLUsage[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLUsage
  = OTIMOFElement.OTIUMLUsage(
    toolSpecific_id = u.toolSpecific_id,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLUseCase[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLUseCase
  = OTIMOFElement.OTIUMLUseCase(
    toolSpecific_id = u.toolSpecific_id,
    isAbstract = u.isAbstract,
    isFinalSpecialization = u.isFinalSpecialization,
    isLeaf = u.isLeaf,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLValuePin[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLValuePin
  = OTIMOFElement.OTIUMLValuePin(
    toolSpecific_id = u.toolSpecific_id,
    isControl = u.isControl,
    isControlType = u.isControlType,
    isLeaf = u.isLeaf,
    isOrdered = u.isOrdered,
    isUnique = u.isUnique,
    name = u.name,
    ordering = u.ordering,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLValueSpecificationAction[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLValueSpecificationAction
  = OTIMOFElement.OTIUMLValueSpecificationAction(
    toolSpecific_id = u.toolSpecific_id,
    isLeaf = u.isLeaf,
    isLocallyReentrant = u.isLocallyReentrant,
    name = u.name,
    visibility = u.visibility)

  def toOTI[Uml <: UML]
  (u : UMLVariable[Uml])
  (implicit ops: UMLOps[Uml])
  : OTIMOFElement.OTIUMLVariable
  = OTIMOFElement.OTIUMLVariable(
    toolSpecific_id = u.toolSpecific_id,
    isOrdered = u.isOrdered,
    isUnique = u.isUnique,
    name = u.name,
    visibility = u.visibility)
}
