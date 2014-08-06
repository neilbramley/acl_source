#' How similar are two joint probability distributions
#'
#' How similar are two joint probability distributions.
#' @param joint The first probability distribution
#' @param joint2 The second probability distribution
#' @param measure 'symmetrised_KL' or 'squared_error', defaults to 'symmetrised_KL'
#' @keywords similarity
#' @export
#' @examples
#' similarity_joints(c(.2,.8), c(.4,.6))

similarity_joints<-function(joint, joint2, measure='symmetrised_KL')
{
  if (measure=='symmetrised_KL')
  {
    tmp<-joint/joint2
    tmp[is.nan(tmp)]<-0
    
    tmp2<-joint2/joint
    tmp2[is.nan(tmp2)]<-0
    
    #adds the two KL divergences KL(joint|joint2) and KL(joint2|joint)
    out<-sum(tmp*joint)+sum(tmp2*joint2)
  }
  else if (measure=='squared_error')
  {
    out<-sum((joint-joint2)^2)
  }
  
  out
}