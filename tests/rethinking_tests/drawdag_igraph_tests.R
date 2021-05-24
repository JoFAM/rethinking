# test code
if (FALSE) {
  
  #Other graph layouts: add_layout_, component_wise, layout_as_bipartite, layout_as_star, layout_as_tree, layout_in_circle, layout_nicely, layout_on_grid, layout_on_sphere, layout_randomly, layout_with_dh, layout_with_fr, layout_with_gem, layout_with_graphopt, layout_with_lgl, layout_with_mds, layout_with_sugiyama, layout_, merge_coords, norm_coords, normalize
  
  library(rethinking)
  
  exdag <- dagitty( "dag {
        U [unobserved]
        X [exposure]
        Y [outcome]
        Z -> X -> Y
        X <- U -> Y
    }")
  
  coordinates(exdag) <- list(x=c(U=1,X=1,Y=2,Z=0),y=c(U=0,X=1,Y=1,Z=1))
  graphdag(exdag,margin=-0.25,edge.curve=0.1 )
  
  l <- graphdag( exdag , layout=layout_in_circle )
  l <- graphdag( exdag , layout=layout_nicely )
  graphdag( exdag , layout=l )
  
  exdag2 <- dagitty( 'dag {
        G [exposure,unobserved]
        P [outcome]
        "G*" <- G -> P -> W -> RG
        RG -> "G*"
    }')
  
  graphdag( exdag2 , edge.curve=0.2 )
  
  l <- sketchdag( exdag2 )
  drawdag( exdag2 , layout=round(l,2) )
  
  sketchdag( exdag2 , asp=0.8 )
  
}
