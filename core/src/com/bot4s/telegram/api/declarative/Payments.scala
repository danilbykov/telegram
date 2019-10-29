package com.bot4s.telegram.api.declarative

import cats.instances.list._
import cats.syntax.applicativeError._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.bot4s.telegram.api.BotBase
import com.bot4s.telegram.methods.AnswerPreCheckoutQuery
import com.bot4s.telegram.models.{PreCheckoutQuery, ShippingQuery}

import scala.collection.mutable

sealed trait PreCheckoutAck
case object SuccessfulPreCheckout extends PreCheckoutAck
case class UnsuccessfulPreCheckout(message: String) extends PreCheckoutAck

/**
  * Declarative interface for processing payments.
  * See [[https://core.telegram.org/bots/payments]].
  */
trait Payments[F[_]] extends BotBase[F] {

  private val shippingQueryActions = mutable.ArrayBuffer[Action[F, ShippingQuery]]()
  private val preCheckoutQueryActions = mutable.ArrayBuffer[Action[F, PreCheckoutQuery]]()
  private val safePreCheckoutQueryActions = mutable.ArrayBuffer[ActionR[F, PreCheckoutQuery, PreCheckoutAck]]()

  /**
    * Executes 'action' for every shipping query.
    */
  def onShippingQuery(action: Action[F, ShippingQuery]): Unit = {
    shippingQueryActions += action
  }

  /**
    * Executes 'action' for every pre-checkout query.
    */
  def onPreCheckoutQuery(action: Action[F, PreCheckoutQuery]): Unit = {
    preCheckoutQueryActions += action
  }

  def onSafePreCheckoutQuery(action: ActionR[F, PreCheckoutQuery, PreCheckoutAck]): Unit =
    safePreCheckoutQueryActions += action

  override def receiveShippingQuery(shippingQuery: ShippingQuery): F[Unit] =
    for {
      _ <- shippingQueryActions.toList.traverse(action => action(shippingQuery))
      _ <- super.receiveShippingQuery(shippingQuery)
    } yield ()

  override def receivePreCheckoutQuery(preCheckoutQuery: PreCheckoutQuery): F[Unit] =
    for {
      _ <- preCheckoutQueryActions.toList.traverse(action => action(preCheckoutQuery))
      _ <-
        safePreCheckoutQueryActions.toList.traverse { safeAction =>
          safeAction(preCheckoutQuery).attempt.flatTap {
            case Right(SuccessfulPreCheckout) =>
              request(AnswerPreCheckoutQuery(preCheckoutQuery.id, true))
            case Right(UnsuccessfulPreCheckout(message)) =>
              request(AnswerPreCheckoutQuery(preCheckoutQuery.id, false, Some(message)))
            case Left(e) =>
              request(AnswerPreCheckoutQuery(preCheckoutQuery.id, false, Some("Internal Error Occurred")))
          }
        }
      _ <- super.receivePreCheckoutQuery(preCheckoutQuery)
    } yield ()
}
