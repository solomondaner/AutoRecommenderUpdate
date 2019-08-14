#' Automate a user's ability to update an entire product recommender with featured SKUs, without duplicating the same SKU per customer.
#' @author Solomon Daner
#' @family Marketing Modeling
#' @param data Product Recommender
#' @param FeaturedProduct This is the product in quotes that you want to feature in the Product Recommender 
#' @param Replaceproduct Logical argument that determines if featured product should replace a product in your table. Set to False
#' @param N The rank of the product that would be replaced. Only applicable if Replaceproduct equals True
#' @param EntityColName Column name in quotes for the Entity, such as customer
#' @param ProductColName Column name in quotes to represent the column name for the product, such as SKU
#' @param Rank Column name in quotes to represent the products rank
#' @param TimeStamp Column name in quotes to represent if the timestamp should be added. If not, use NULL. Automatically shows if ReplaceProduct equals True.
#' @return The product recommender with featured products 
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
  TimeStamp = "ModelTimeStamp") {
  
  # Ensure data is data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Gather Data to Determine Correct Scenerio----
  CustomersWithFeaturedItem <- data[get(ProductColName) == FeaturedProduct, get(EntityColName)]
  CustomersWithFeaturedItemTable <- data[get(EntityColName) %in% CustomersWithFeaturedItem]
  CustomersWithoutFeaturedItemTable <- data[!(get(EntityColName) %in% CustomersWithFeaturedItem )]
  
  # The Three Possible Scenerios are Listed Below----
  # No Customer Has the Fetured Product Recommended to Them
  # Every Customer Already Has the Fetured Product Recommended to Them
  # Some Customers Have the Fetured Product Recommended to Them
  if (nrow(CustomersWithFeaturedItemTable) == 0) {
    A <- CustomersWithoutFeaturedItemTable
    B <- NULL
  } else if(nrow(CustomersWithoutFeaturedItemTable) == 0) {
    return ("Every Customer has the featured product")
  } else {
    A <- CustomersWithoutFeaturedItemTable
    B <- CustomersWithFeaturedItemTable
  }
  
  # Choose to replace the least likey product with the featured Product----
  if(ReplaceProduct == TRUE) { 
    if(!N %in% unique(A[,get(Rank)])) {
      stop("Rank does not exist in model")
    }
    data.table::set(
      A, 
      i = A[get(Rank) == N, which = TRUE],
      j = eval(ProductColName), 
      value = FeaturedProduct) 
    data <- data.table::rbindlist(
      list(A,B), 
      fill = TRUE)
    data.table::setorderv(
      data, 
      cols = c(eval(EntityColName),
               eval(Rank)))
    return(data)
    
    # Add the featured product to current recommendations----
  } else {
    
    # Test if timestamp should be added----
    if(is.null(TimeStamp)){
      DT <- data.table::data.table(
        V1 = unique(A[,get(EntityColName)]), 
        V2 = FeaturedProduct,
        V3 = 0)
      data.table::setnames(
        x = DT, 
        old = c("V1",
                "V2",
                "V3"),
        new = c(eval(EntityColName),
                eval(ProductColName),
                eval(Rank)))
    } else {
      DT <- data.table::data.table(
        V1 = unique(A[,get(EntityColName)]), 
        V2 = FeaturedProduct,
        V3 = 0, 
        V4 = unique(A[,get(TimeStamp)]))
      data.table::setnames(
        x = DT, 
        old = c("V1",
                "V2",
                "V3",
                "V4"),
        new = c(eval(EntityColName),
                eval(ProductColName),
                eval(Rank),
                eval(TimeStamp)))
    }
    
    # Finalize data prep and return----
    data <- data.table::rbindlist(
      list(DT,A,B), 
      fill = TRUE)
    data.table::setorderv(
      data, 
      cols = c(eval(EntityColName),
               eval(Rank)))
    return(data)
  }
}
