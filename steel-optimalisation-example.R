library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(dplyr)


inventoryTable <- data.frame(
    ID =  sapply(1:10, function(x) paste(sample(0:9, 5, replace = TRUE), collapse = "")),
    Dimension = c(rep("HUP 120x120x5", 5), rep("HUP 150x150x5", 5)),
    Length = c(1000, 2000, 3000, 4000, 5000, 1000, 2000, 3000, 4000, 5000),
    Surface = c(rep("Black", 5), rep("Primed", 5))
)


workTable <- data.frame(
    JobName = c("Job 1", "Job 2", "Job 3", "Job 4", "Job 5"),
    Dimension = c("HUP 120x120x5", "HUP 120x120x5", "HUP 150x150x5", "HUP 150x150x5", "HUP 150x150x5"),
    Surface = c("Black", "Black", "Primed", "Primed", "Primed"),
    Length = c(500, 1000, 1500, 750, 2000)
)


numberOfWorkItems <- nrow(workTable)
numberOfInventoryItems <- nrow(inventoryTable)
bigMNumber <- sum(as.numeric(workTable$Length)) * 1.1

model <- MIPModel() |>
    # Binary decision variable defining the steel to be cut
    add_variable(cutSteel[work, inventory],
        work = 1:numberOfWorkItems,
        inventory = 1:numberOfInventoryItems,
        type = "binary"
    ) |>
    # Binary decision variable: Take new item from inventory?
    add_variable(takeItem[inventory],
        inventory = 1:numberOfInventoryItems,
        type = "binary"
    ) |>
    # Binary variable: Does the type of work item and inventory item match?
    add_variable(typeMatch[work, inventory],
        work = 1:numberOfWorkItems,
        inventory = 1:numberOfInventoryItems,
        type = "binary"
    ) |>
    # Binary variable: Does the surface of work item and inventory item match?
    add_variable(surfaceMatch[work, inventory],
        work = 1:numberOfWorkItems,
        inventory = 1:numberOfInventoryItems,
        type = "binary"
    ) |>
    # Constraint 1: Each item in workTable must be cut
    add_constraint(
        sum_over(cutSteel[work, inventory],
            inventory = 1:numberOfInventoryItems
        ) == 1,
        work = 1:numberOfWorkItems
    ) |>
    # Constraint 2: The sum of each item used to cut from needs to be equal to or smaller than the work item #nolint
    add_constraint(
        sum_over(cutSteel[work, inventory] * workTable$Length[work],
            work = 1:numberOfWorkItems
        ) <= inventoryTable$Length[inventory],
        inventory = 1:numberOfInventoryItems
    ) |>
    # Constraint 3: bigMNumber constraint to activate takeItem whenever a length is cut #nolint
    add_constraint(
        sum_over(cutSteel[work, inventory],
            work = 1:numberOfWorkItems
        ) <= bigMNumber * takeItem[inventory],
        inventory = 1:numberOfInventoryItems
    ) |>
    # Constraint 4: typeMatch can only be 1 if types match
    add_constraint(
        typeMatch[work, inventory] == (workTable$Dimension[work] == inventoryTable$Dimension[inventory]), # nolint
        work = 1:numberOfWorkItems, inventory = 1:numberOfInventoryItems
    ) |>
    # Constraint 5: surfaceMatch can only be 1 if surfaces match
    add_constraint(
        surfaceMatch[work, inventory] == (workTable$Surface[work] == inventoryTable$Surface[inventory]), # nolint
        work = 1:numberOfWorkItems, inventory = 1:numberOfInventoryItems
    ) |>
    # Constraint 6: cutSteel can only be 1 if typeMatch and surfaceMatch are 1 #nolint
    add_constraint(
        cutSteel[work, inventory] <= typeMatch[work, inventory],
        work = 1:numberOfWorkItems, inventory = 1:numberOfInventoryItems
    ) |>
    add_constraint(
        cutSteel[work, inventory] <= surfaceMatch[work, inventory],
        work = 1:numberOfWorkItems, inventory = 1:numberOfInventoryItems
    ) |>
    # Set objective function to minimize scrap / waste
    set_objective(
        sum_over(
            takeItem[inventory] * inventoryTable$Length[inventory],
            inventory = 1:numberOfInventoryItems
        )
        - sum_over(cutSteel[work, inventory] * workTable$Length[work],
                work = 1:numberOfWorkItems,
                inventory = 1:numberOfInventoryItems
            ),
        sense = "min"
    )


# Solve the model
result <- solve_model(model, with_ROI(solver = "glpk"))

# Get the solution
solution <- get_solution(result, cutSteel[work, inventory]) |>
    filter(value > 0) |>
    mutate(cutLength = workTable$Length[work])


productionPlan <- solution |>
    select(-c(variable, value)) |>
    arrange(work) |>
    mutate(
        JobName = workTable$JobName[work],
        Dimension = workTable$Dimension[work],
        Surface = workTable$Surface[work],
        Length = workTable$Length[work],
        InventoryID = inventoryTable$ID[inventory],
        `Start length` = inventoryTable$Length[inventory],
        `Cut length` = cutLength,
        `Leftover` = inventoryTable$Length[inventory] - cutLength,
    ) |>
    select(-c(work, inventory, cutLength))



