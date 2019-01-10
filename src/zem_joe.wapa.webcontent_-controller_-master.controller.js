sap.ui.define([                                                                                                                                                                                                                                                
	"zemhr/xx/SAPUI5Training/controller/BaseController",                                                                                                                                                                                                          
	"sap/ui/Device"                                                                                                                                                                                                                                               
], function(BaseController, Device) {                                                                                                                                                                                                                          
	"use strict";                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                               
	return BaseController.extend("zemhr.xx.SAPUI5Training.controller.Master", {                                                                                                                                                                                   
                                                                                                                                                                                                                                                               
		oDeferred : null,&nbsp;                                                                                                                                                                                                                                      
		                                                                                                                                                                                                                                                             
		onInit : function() {                                                                                                                                                                                                                                        
			BaseController.prototype.onInit.apply(this, arguments);                                                                                                                                                                                                     
			                                                                                                                                                                                                                                                            
			this._initController();                                                                                                                                                                                                                                     
			                                                                                                                                                                                                                                                            
			this._initData();                                                                                                                                                                                                                                           
			                                                                                                                                                                                                                                                            
			this.getRouter().getTargets().display("notFound");                                                                                                                                                                                                          
			                                                                                                                                                                                                                                                            
			this.getRouter().attachRouteMatched(this._onRouteMatched, this);                                                                                                                                                                                            
		},                                                                                                                                                                                                                                                           
		                                                                                                                                                                                                                                                             
		onNavDetail : function(oEvent) {                                                                                                                                                                                                                             
			this._toDetail(oEvent.getParameter("listItem") || oEvent.getSource());                                                                                                                                                                                      
		},                                                                                                                                                                                                                                                           
		                                                                                                                                                                                                                                                             
		selectDefaultItem : function() {                                                                                                                                                                                                                             
			var aItems = [], oItem;                                                                                                                                                                                                                                     
			                                                                                                                                                                                                                                                            
			if (!Device.system.phone) {                                                                                                                                                                                                                                 
				aItems = this.oList.getItems();                                                                                                                                                                                                                            
				if (aItems.length) {                                                                                                                                                                                                                                       
					oItem = this.oList.getSelectedItem() || aItems[0];                                                                                                                                                                                                        
					this.oList.setSelectedItem(oItem, true);                                                                                                                                                                                                                  
				}                                                                                                                                                                                                                                                          
			}                                                                                                                                                                                                                                                           
			this._toDetail();                                                                                                                                                                                                                                           
		},                                                                                                                                                                                                                                                           
		                                                                                                                                                                                                                                                             
		setlectItemFromId : function(sId) {                                                                                                                                                                                                                          
			var i, bFound = false, aItems, oItem, oData;                                                                                                                                                                                                                
			                                                                                                                                                                                                                                                            
			aItems = this.oList.getItems();                                                                                                                                                                                                                             
			for (i = 0; i < aItems.length && !bFound; i++) {                                                                                                                                                                                                            
				oItem = aItems[i];&nbsp;                                                                                                                                                                                                                                   
				if (oItem.getBindingContext("employee").getProperty("Pernr") === sId) {                                                                                                                                                                                    
					this.oList.setSelectedItem(oItem, true);                                                                                                                                                                                                                  
					bFound = true;                                                                                                                                                                                                                                            
				}                                                                                                                                                                                                                                                          
			}                                                                                                                                                                                                                                                           
			if (!bFound) {                                                                                                                                                                                                                                              
				this.oList.removeSelections(true);                                                                                                                                                                                                                         
			}                                                                                                                                                                                                                                                           
			this._toDetail();                                                                                                                                                                                                                                           
		},                                                                                                                                                                                                                                                           
		                                                                                                                                                                                                                                                             
		_toDetail : function(oItem) {                                                                                                                                                                                                                                
			if (!oItem) {                                                                                                                                                                                                                                               
				oItem = this.oList.getSelectedItem();                                                                                                                                                                                                                      
			}                                                                                                                                                                                                                                                           
			if (oItem) {                                                                                                                                                                                                                                                
				this.getRouter().navTo("detail", {                                                                                                                                                                                                                         
					employeeNumber : oItem.getBindingContext("employee").getProperty("Pernr")                                                                                                                                                                                 
				}, Device.system.phone);                                                                                                                                                                                                                                   
			}                                                                                                                                                                                                                                                           
		},                                                                                                                                                                                                                                                           
		                                                                                                                                                                                                                                                             
		_initController : function() {                                                                                                                                                                                                                               
			var that = this;                                                                                                                                                                                                                                            
			                                                                                                                                                                                                                                                            
			this.oList = this.byId("masterList");                                                                                                                                                                                                                       
			                                                                                                                                                                                                                                                            
			this.oList.addDelegate({                                                                                                                                                                                                                                    
				onAfterRendering: function(oEvent) {                                                                                                                                                                                                                       
					that.selectDefaultItem();                                                                                                                                                                                                                                 
				}                                                                                                                                                                                                                                                          
			});                                                                                                                                                                                                                                                         
		},                                                                                                                                                                                                                                                           
		                                                                                                                                                                                                                                                             
		_initData : function() {                                                                                                                                                                                                                                     
			var oModel;                                                                                                                                                                                                                                                 
			                                                                                                                                                                                                                                                            
			/* Initial deferred object */                                                                                                                                                                                                                               
			this.oDeferred = jQuery.Deferred();                                                                                                                                                                                                                         
			                                                                                                                                                                                                                                                            
//			oModel = this.getModel("employee");                                                                                                                                                                                                                       
//			oModel.setData({                                                                                                                                                                                                                                          
//				results : {                                                                                                                                                                                                                                              
//					"00000001" : { Pernr : "00000001", Bukrs : "TH01", Plans : "09501410", Ename : "Mr Adam Wright" },                                                                                                                                                      
//					"00000002" : { Pernr : "00000002", Bukrs : "TH01", Plans : "09500000", Ename : "Miss Tim Taylor" },                                                                                                                                                     
//					"00000003" : { Pernr : "00000003", Bukrs : "BE21", Plans : "07891001", Ename : "Miss WSR BE" },                                                                                                                                                         
//					"00000004" : { Pernr : "00000004", Bukrs : "TH02", Plans : "01240890", Ename : "Mr Kris Redfield" },                                                                                                                                                    
//					"00000013" : { Pernr : "00000013", Bukrs : "TH03", Plans : "01240898", Ename : "Mr Retro Testo" }                                                                                                                                                       
//				}                                                                                                                                                                                                                                                        
//			});                                                                                                                                                                                                                                                       
			                                                                                                                                                                                                                                                            
			/* Trigger ready for waiting logic */                                                                                                                                                                                                                       
			this.oDeferred.resolve();                                                                                                                                                                                                                                   
		},                                                                                                                                                                                                                                                           
		                                                                                                                                                                                                                                                             
		_onRouteMatched : function(oEvent) {                                                                                                                                                                                                                         
			var sName, oArguments;                                                                                                                                                                                                                                      
			                                                                                                                                                                                                                                                            
			sName = oEvent.getParameter("name");                                                                                                                                                                                                                        
			oArguments = oEvent.getParameter("arguments");                                                                                                                                                                                                              
			                                                                                                                                                                                                                                                            
			jQuery.when(this.oDeferred).then(jQuery.proxy(function() {                                                                                                                                                                                                  
				/* On the empty hash select the first item */                                                                                                                                                                                                              
				if (sName === "master" && !oArguments.employeeNumber) {                                                                                                                                                                                                    
					this.selectDefaultItem();                                                                                                                                                                                                                                 
				} else {                                                                                                                                                                                                                                                   
					/* Select item from list */                                                                                                                                                                                                                               
					this.setlectItemFromId(oArguments.employeeNumber);                                                                                                                                                                                                        
				}                                                                                                                                                                                                                                                          
			}, this));                                                                                                                                                                                                                                                  
		}                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                               
	});                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                               
});                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                               