box::use(
    AzureStor,
    rlang,
    glue
)


#' @export
load_proj_containers <- function() {
    es <- azure_endpoint_url()
    # storage endpoint
    se <- AzureStor$storage_endpoint(es, sas = Sys.getenv("DSCI_AZ_SAS_DEV"))
    # storage container
    sc_global <- AzureStor$storage_container(se, "global")
    sc_projects <- AzureStor$storage_container(se, "projects")
    list(
        GLOBAL_CONT = sc_global,
        PROJECTS_CONT = sc_projects
    )
}

azure_endpoint_url <- function(
        service = c("blob", "file"),
        stage = c("dev", "prod"),
        storage_account = "imb0chd0") {
    blob_url <- "https://{storage_account}{stage}.{service}.core.windows.net/"
    service <- rlang$arg_match(service)
    stage <- rlang$arg_match(stage)
    storae_account <- rlang$arg_match(storage_account)
    endpoint <- glue$glue(blob_url)
    return(endpoint)
}