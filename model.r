#Linear Regression


linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])
summary(linear_reg_mod)

linear_reg_mod2 = lm(Item_Outlet_Sales ~ Item_MRP+Outlet_IdentifierOUT013+Outlet_IdentifierOUT017+Outlet_IdentifierOUT018+Outlet_IdentifierOUT027+Outlet_IdentifierOUT035+Outlet_IdentifierOUT045+Outlet_IdentifierOUT046+Outlet_IdentifierOUT049, data = train[,-c("Item_Identifier")])
summary(linear_reg_mod2)

## predicting on test set and writing a submission file
submission$Item_Outlet_Sales = predict(linear_reg_mod2, test[,-c("Item_Identifier")])
write.csv(submission, "Linear_Reg_submit.csv", row.names = F)
#-------------------------------------------------------------------------------------------------------------------------
## Lasso Regression
set.seed(1235)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002))

lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,
                       method='glmnet', trControl= my_control, tuneGrid = Grid)

# mean validation score
mean(lasso_linear_reg_mod$resample$RMSE)

#-------------------------------------------------------------------------------------------------------------------------
## Ridge Regression
set.seed(1236)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002))

ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,
                       method='glmnet', trControl= my_control, tuneGrid = Grid)

# mean validation score
mean(ridge_linear_reg_mod$resample$RMSE)

#-------------------------------------------------------------------------------------------------------------------------
## RandomForest Model
set.seed(1237)
my_control = trainControl(method="cv", number=5)

tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)

rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], 
               y = train$Item_Outlet_Sales,
               method='ranger', 
               trControl= my_control, 
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")

# mean validation score
mean(rf_mod$resample$RMSE)

## plot displaying RMSE scores for different tuning parameters
plot(rf_mod)

## plot variable importance
plot(varImp(rf_mod))

#-------------------------------------------------------------------------------------------------------------------------
## List of parameters for XGBoost modeling
param_list = list(
        objective = "reg:linear",
        eta=0.01,
        gamma = 1,
        max_depth=6,
        subsample=0.8,
        colsample_bytree=0.5
        )

## converting train and test into xgb.DMatrix format

dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), label= train$Item_Outlet_Sales)
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))

## 5-fold cross-validation to find optimal value of nrounds

set.seed(112)
xgbcv = xgb.cv(params = param_list, 
               data = dtrain, 
               nrounds = 1000, 
               nfold = 5, 
               print_every_n = 10, 
               early_stopping_rounds = 30, 
               maximize = F)

## training XGBoost model at nrounds = 428
xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 428)

## Variable Importance
var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), 
                         model = xgb_model)

xgb.plot.importance(var_imp)

