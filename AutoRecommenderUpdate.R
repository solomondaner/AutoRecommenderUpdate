#'Automatically feature products in your current reccommender model
#' @author Solomon Daner
#' @family Marketing Modeling
#' @param data The scored model from AutoRecomScoring()
#' @param FeaturedProduct This is the product in quotes that you want to feature 
#' @param Replaceproduct Logical argument that determines if featured product should replace the product your table. Set to False
#' @param N The ranks of the product that would be replaced 
#' @param EntityColName Column name in quotes for the Entity, such as customer
#' @param ProductColName Column name in quotes t0 represent the column name for the product, such as SKU
#' @param Rank Column name in quotes to represent the products rank
#' @param TimeStamp Column name in quotes to represent if the timestamp should be added. If not, use ""
#' @return The scored model with featured products 
#' @examples 
#' @export
AutoRecommenderUpdate <- function (
  data,
  FeaturedProduct,
  ReplaceProduct = FALSE,
  N = 10,
  EntityColName  = "CustomerID",
  ProductColName = "StockCode",
  Rank = "ProductRank",
  TimeStamp = "ModelTimeStamp"
) {
  
  # Ensure data is data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Gather Data to Determine Correct Scenerio----
  CustomersWithFeaturedItem <- data[get(ProductColName) == FeaturedProduct, get(EntityColName)]
  CustomersWithFeaturedItemTable <- data[get(EntityColName) %in% CustomersWithFeaturedItem]
  CustomersWithoutFeaturedItemTable <- data[!(get(EntityColName) %in% CustomersWithFeaturedItem )]
  
  # The Three Possible Scenerios are Listed Below----
  # No Customer Has the Fetured Product Recommended to Them----
  # Every Customer Already Has the Fetured Product Recommended to Them----
  # Some Customers Have the Fetured Product Recommended to Them----
  if (nrow(CustomersWithFeaturedItemTable) == 0) {
    A = CustomersWithoutFeaturedItemTable
    B = NULL
  } else if(nrow(CustomersWithoutFeaturedItemTable) == 0) {
    stop ("Every Customer has the featured product")
  } else {
    A = CustomersWithoutFeaturedItemTable
    B = CustomersWithFeaturedItemTable
  }
  
  # Choose to replace the least likey product with the featured Product----
  if(ReplaceProduct == TRUE) { 
    if(!N %in% unique(A[,get(Rank)])) {
      stop("Rank does not exist in model")
    }
    data.table :: set(A,A[get(Rank) == N, which = TRUE],
                      get("ProductColName"),FeaturedProduct) 
    data <- data.table :: rbindlist(list(A,B), fill = TRUE)
    data.table :: setorderv(data,c(get("EntityColName"),get("Rank")))
    
    # Add the faetured product to current recommendations----
  } else {
    
    # Test if timestamp should be added----
  if(TimeStamp == ""){
      DT <- data.table :: data.table(unique(A[,get(EntityColName)]), FeaturedProduct,0)
      names(DT) <- c(get("EntityColName"),get("ProductColName"),get("Rank"))
  } else {
      DT <- data.table :: data.table(unique(A[,get(EntityColName)]), FeaturedProduct,0, unique(A[,get(TimeStamp)]))
      names(DT) <- c(get("EntityColName"),get("ProductColName"),get("Rank"), get("TimeStamp"))
  }
    data <- data.table :: rbindlist(list(DT,A,B),fill = TRUE)
    data.table :: setorderv(data,c(get("EntityColName"),get("Rank")))
  }}


