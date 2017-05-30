//////////////////////////////// Interface /////////////////////////////////////
//------------------------------------
//APB (Advanced peripheral Bus) Interface 
//
//------------------------------------
`ifndef __APB_IF_SV__
`define __APB_IF_SV__

interface apb_if(input logic pclk, input logic prst);

   logic [31:0] paddr	;
   logic        psel	;
   logic        penable	;
   logic        pwrite	;
   logic [31:0] prdata	;
   logic [31:0] pwdata	;


   //Master Clocking block - used for Drivers
   clocking master_cb @(posedge pclk);
      output paddr, psel, penable, pwrite, pwdata;
      input  prdata;
   endclocking: master_cb

   //Slave Clocking Block - used for any Slave BFMs
   clocking slave_cb @(posedge pclk);
      input  paddr, psel, penable, pwrite, pwdata;
      output prdata;
   endclocking: slave_cb

   //Monitor Clocking block - For sampling by monitor components
   clocking monitor_cb @(posedge pclk);
      input paddr, psel, penable, pwrite, prdata, pwdata;
   endclocking: monitor_cb

  modport master(clocking master_cb);
  modport slave(clocking slave_cb);
  modport passive(clocking monitor_cb);

endinterface: apb_if

`endif  // __APB_IF_SV__



//////////////////////////////// Top module ////////////////////////////////////
import uvm_pkg::*;
`include "uvm_macros.svh"

 //Include all files
`include "mips_if.sv"
`include "mips_config.sv"
`include "mips_seq_item.sv"
`include "mips_sequence.sv"
`include "mips_sequencer.sv"
`include "mips_driver.sv"
`include "mips_monitor.sv"
`include "mips_agent.sv"
`include "mips_env.sv"
`include "mips_test.sv"
//`include "design.sv"

//--------------------------------------------------------
//Top level module that instantiates  just a physical apb interface
//No real DUT or APB slave as of now
//--------------------------------------------------------
module tb_top;

   	logic pclk;
   	logic prst; 
   
   	logic [31:0] paddr  ;
	logic        psel   ;
	logic        penable;
	logic        pwrite ;
	logic [31:0] pwdata ;
	logic [31:0] prdata ;
  
   //clk gen
   	initial begin
		pclk=0;
      	forever begin
      		#10 pclk = ~pclk;
      	end
   	end
	
   	//reset block
   	initial begin
   		uvm_event reset_done = new("reset_done"); 
   		uvm_config_db#(uvm_event)::set( null, "*", "reset_done", reset_done);
   		prst = 0; 
   		repeat(20) @(posedge pclk);
   		prst = 1;
   		repeat(20) @(posedge pclk);
   		prst = 0;
   		reset_done.trigger();
   	end
	
   	//Instantiate a physical interface for APB interface
  	apb_if  apb_if_inst(.pclk(pclk), .prst(prst));
 
   	//instantiate dut
   	apb_dut apb_dut_inst(	.pclk	(pclk   ), 
   							.prst	(prst   ),
   							.paddr	(paddr	),
							.psel	(psel	),
							.penable(penable),	
							.pwrite	(pwrite	),
							.pwdata	(pwdata	),
							.prdata (prdata )
   						);
   	
   	//connect interface
   	assign paddr	= apb_if_inst.paddr	   ; 
    assign psel		= apb_if_inst.psel	   ;
    assign penable	= apb_if_inst.penable  ;
   	assign pwrite	= apb_if_inst.pwrite   ;  	
   	assign pwdata	= apb_if_inst.pwdata   ;
   	assign apb_if_inst.prdata	= prdata   ;
   	
  	initial begin
    	//Pass this physical interface to test top (which will further pass it down to env->agent->drv/sqr/mon
    	uvm_config_db#(virtual apb_if)::set( null, "uvm_test_top", "vif", apb_if_inst);
    
    	//Call the test - but passing run_test argument as test class name
    	//Another option is to not pass any test argument and use +UVM_TEST on command line to sepecify which test to run
    	run_test("apb_base_test");
  	end
  
   initial begin
    $dumpfile("dump.vcd"); 
    $dumpvars;
   end
endmodule



/////////////////////////// Test /////////////////////////////
`ifndef __APB_BASE_TEST_SV__
`define __APB_BASE_TEST_SV__

//--------------------------------------------------------
//Top level Test class that instantiates env, configures and starts stimulus
//--------------------------------------------------------
class apb_base_test extends uvm_test;

  //Register with factory
  `uvm_component_utils(apb_base_test);
  
  apb_env  	 env;
  apb_config cfg;
  uvm_event  reset_done; 
  virtual apb_if vif;
  
  function new(string name = "apb_base_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  //Build phase - Construct the cfg and env class using factory
  //Get the virtual interface handle from Test and then set it config db for the env component
  function void build_phase(uvm_phase phase);
    cfg = apb_config::type_id::create("cfg", this);
    env = apb_env::type_id::create("env", this);
    //
    if (!uvm_config_db#(virtual apb_if)::get(this, "", "vif", vif)) begin
       `uvm_fatal(this.get_name(), "No virtual interface specified for this test instance")
    end 
    uvm_config_db#(virtual apb_if)::set( this, "env", "vif", vif);
    
    //pass down to any component starting from 'this' level
    uvm_config_db#(apb_config)::set( this, "*", "cfg", cfg);
    
    if(!uvm_config_db#(uvm_event)::get( null, "*", "reset_done", reset_done))begin
	    `uvm_fatal(this.get_name(), "No reset_done specified for this test instance")
    end
    
  endfunction

  //Run phase - Create an abp_sequence and start it on the apb_sequencer
  task run_phase( uvm_phase phase );
    apb_base_seq apb_seq;
    apb_seq = apb_base_seq::type_id::create("apb_seq");
    
    phase.raise_objection( this, "Starting apb_base_seqin main phase" );
    reset_done.wait_trigger();
    $display("%t Starting sequence apb_seq run_phase",$time);
    apb_seq.start(env.agt.sqr);
    #100ns;
    phase.drop_objection( this , "Finished apb_seq in main phase" );
  endtask: run_phase
  
  
endclass


`endif


///////////////////////////////// Environment ///////////////////////////////////
//----------------------------------------------                                                        
// APB Env class                                                                                        
//----------------------------------------------

`ifndef __APB_ENV__
`define __APB_ENV__
                                                        
class apb_env  extends uvm_env;                                                                         
                                                                                                        
   `uvm_component_utils(apb_env);                                                                       
                                                                                                        
   //ENV class will have agent as its sub component                                                     
   apb_agent  agt;  
   apb_scoreboard scb;                                                                                     
   //virtual interface for APB interface                                                                
   virtual apb_if  vif;                                                                                 
                                                                                                        
   function new(string name, uvm_component parent = null);                                              
      super.new(name, parent);                                                                          
   endfunction                                                                                          
                                                                                                        
   //Build phase - Construct agent and get virtual interface handle from test  and pass it down to agent
   function void build_phase(uvm_phase phase);   
   	 super.build_phase(phase);   
   	    
     scb = apb_scoreboard::type_id::create("scb", this);                                                  
     agt = apb_agent::type_id::create("agt", this); 
                                                         
     if (!uvm_config_db#(virtual apb_if)::get(this, "", "vif", vif)) begin                              
         `uvm_fatal("APB/AGT/NOVIF", "No virtual interface specified for this env instance")            
     end 
     
     //pass it down to agent                                                                                               
     uvm_config_db#(virtual apb_if)::set( this, "agt", "vif", vif);                                     
   endfunction: build_phase  
   
   //Connect - driver and monitor analysis port to scoreboard                                            
   virtual function void connect_phase(uvm_phase phase);                                      
      agt.drv.Drvr2Sb_port.connect(scb.drv2Sb_port);                                         
      uvm_report_info("apb_agent::", "connect_phase, Connected driver to scb"); 
      
      agt.mon.ap.connect(scb.mon2Sb_port);                                         
      uvm_report_info("apb_agent::", "connect_phase, Connected monitor to scb");         
   endfunction                                                                             
                                                                                                        
endclass : apb_env                                                                                      
                                                                                                        
                                                                                                        
`endif  //__APB_ENV__          


///////////////////////////////////////// Agent /////////////////////////////////////
//---------------------------------------                                                     
// APB Agent class                                                                            
//---------------------------------------   

`ifndef __APB_AGENT__
`define __APB_AGENT__

                                                  
class apb_agent extends uvm_agent;                                                            
                                                                                              
   //Agent will have the sequencer, driver and monitor components for the APB interface       
   apb_sequencer 	sqr;                                                                         
   apb_master_drv 	drv;                                                                        
   apb_monitor 		mon;                                                                           
                                                                                              
   virtual apb_if  vif;                                                                       
                                                                                              
   `uvm_component_utils_begin(apb_agent)                                                      
      `uvm_field_object(sqr, UVM_ALL_ON)                                                      
      `uvm_field_object(drv, UVM_ALL_ON)                                                      
      `uvm_field_object(mon, UVM_ALL_ON)                                                      
   `uvm_component_utils_end                                                                   
                                                                                              
   function new(string name, uvm_component parent = null);                                    
      super.new(name, parent);                                                                
   endfunction                                                                                
                                                                                              
   //Build phase of agent - construct sequencer, driver and monitor                           
   //get handle to virtual interface from env (parent) config_db                              
   //and pass handle down to srq/driver/monitor                                               
   virtual function void build_phase(uvm_phase phase);                                        
      sqr = apb_sequencer::type_id::create("sqr", this);                                      
      drv = apb_master_drv::type_id::create("drv", this);                                     
      mon = apb_monitor::type_id::create("mon", this);                                        
                                                                                              
      if (!uvm_config_db#(virtual apb_if)::get(this, "", "vif", vif)) begin                   
         `uvm_fatal("APB/AGT/NOVIF", "No virtual interface specified for this agent instance")
      end  
                                                                                         
     uvm_config_db#(virtual apb_if)::set( this, "drv", "vif", vif);                           
     uvm_config_db#(virtual apb_if)::set( this, "mon", "vif", vif);
                                
   endfunction: build_phase                                                                   
                                                                                              
   //Connect - driver and sequencer port to export                                            
   virtual function void connect_phase(uvm_phase phase);                                      
      drv.seq_item_port.connect(sqr.seq_item_export);                                         
      uvm_report_info("apb_agent::", "connect_phase, Connected driver to sequencer");         
   endfunction                                                                                
endclass: apb_agent        

`endif // __APB_AGENT__              


///////////////////////////////////// Sequence item ///////////////////////////
//------------------------------------
// Basic APB  Read/Write Transaction class definition
//  This transaction will be used by Sequences, Drivers and Monitors
//------------------------------------
`ifndef __APB_RW_SEQ_ITEM__
`define __APB_RW_SEQ_ITEM__

//import uvm_pkg::*;

//apb_rw sequence item derived from base uvm_sequence_item
class apb_rw_seq_item extends uvm_sequence_item;
 
   //typedef for READ/Write transaction type
   typedef enum {READ, WRITE} kind_e;
   rand bit   [31:0] addr;      //Address
   rand logic [31:0] data;     //Data - For write or read response
   rand kind_e  apb_cmd;       //command type

    //Register with factory for dynamic creation
   `uvm_object_utils(apb_rw_seq_item)
  
   
   function new (string name = "apb_rw_seq_item");
      super.new(name);
   endfunction

   function string convert2string();
     return $psprintf("kind=%s addr=%0h data=%0h ",apb_cmd,addr,data);
   endfunction

endclass: apb_rw_seq_item

`endif//__APB_RW_SEQ_ITEM__




///////////////////////////////////// Sequence ////////////////////////////////  
//A few flavours of apb sequences

`ifndef __APB_SEQUENCE_SV__
`define __APB_SEQUENCE_SV__

//------------------------
//Base APB sequence derived from uvm_sequence and parameterized with sequence item of type apb_rw
//------------------------
class apb_base_seq extends uvm_sequence#(apb_rw_seq_item);

  `uvm_object_utils(apb_base_seq)

  function new(string name ="");
    super.new(name);
  endfunction


  //Main Body method that gets executed once sequence is started
  task body();
     apb_rw_seq_item rw_trans;
     //Create 10 random APB read/write transaction and send to driver
     repeat(10) begin
       rw_trans = apb_rw_seq_item::type_id::create(.name("rw_trans"),.contxt(get_full_name()));
       start_item(rw_trans);
       assert (rw_trans.randomize());
       finish_item(rw_trans);
     end
  endtask
  
endclass

`endif//__APB_SEQUENCE_SV__
////////////////////////////////////
//wait_for_grant(); 
//req.inst = instruction::PUSH_B; 
//send_request(req); 
//wait_for_item_done(); 
//get_response(res);
//////////////////////////////// 

///////////////////////////////////// Sequencer ///////////////////////////////
//---------------------------------------------
// APB Sequencer Class  
//  Derive form uvm_sequencer and parameterize to apb_rw sequence item
//---------------------------------------------
`ifndef __APB_SEQUENCER_SV__
`define __APB_SEQUENCER_SV__

class apb_sequencer extends uvm_sequencer #(apb_rw_seq_item);

   `uvm_component_utils(apb_sequencer)
 
   function new(input string name, uvm_component parent=null);
      super.new(name, parent);
   endfunction : new

endclass : apb_sequencer
`endif//__APB_SEQUENCER_SV__


/////////////////////////////////// Driver //////////////////////////////////
//---------------------------------------------
// APB master driver Class  
//---------------------------------------------

`ifndef __APB_DRIVER__
`define __APB_DRIVER__

//forward declaration
typedef apb_agent;

class apb_master_drv extends uvm_driver#(apb_rw_seq_item);
  
  `uvm_component_utils(apb_master_drv)
   
   virtual apb_if vif;
   
   //config class handle
   apb_config cfg;
   uvm_event  reset_done; 
	uvm_analysis_port #(apb_rw_seq_item) Drvr2Sb_port;

   function new(string name,uvm_component parent = null);
      super.new(name,parent);
   endfunction

   //Build Phase
   //Get the virtual interface handle form the agent (parent ) or from config_db
   function void build_phase(uvm_phase phase);
     apb_agent agent;
     super.build_phase(phase);
     
     Drvr2Sb_port = new("Drvr2Sb", this);
     
     if ($cast(agent, get_parent()) && agent != null) begin
         vif = agent.vif;
     end
     else begin
         if (!uvm_config_db#(virtual apb_if)::get(this, "", "vif", vif)) begin
            `uvm_fatal("APB/DRV/NOVIF", "No virtual interface specified for this driver instance")
         end
     end
     
    if(!uvm_config_db#(uvm_event)::get( null, "*", "reset_done", reset_done))begin
	    `uvm_fatal(this.get_name(), "No reset_done specified for this test instance")
    end
   endfunction

   //Run Phase
   //Implement the Driver -Sequencer API to get an item
   //Based on if it is Read/Write - drive on APB interface the corresponding pins
   virtual task run_phase(uvm_phase phase);
     super.run_phase(phase);
     this.vif.master_cb.psel    <= '0;
     this.vif.master_cb.penable <= '0;

     reset_done.wait_trigger();
     
     forever begin
       apb_rw_seq_item tr;
       @ (this.vif.master_cb);
       
       //First get an item from sequencer
       seq_item_port.get_next_item(tr);
       Drvr2Sb_port.write(tr);
       
       
       @ (this.vif.master_cb);
       
       
       //Decode the APB Command and call either the read/write function
       case (tr.apb_cmd)
         apb_rw_seq_item::READ:  begin
         	drive_read(tr.addr, tr.data);  
         	uvm_report_info("APB_DRIVER ", $psprintf("Got Transaction %s",tr.convert2string()));
         end
         apb_rw_seq_item::WRITE: begin
         	uvm_report_info("APB_DRIVER ", $psprintf("Got Transaction %s",tr.convert2string()));
         	drive_write(tr.addr, tr.data);
         end
       endcase
       
       
       //Handshake DONE back to sequencer
       seq_item_port.item_done();
       
     end
   endtask: run_phase

   virtual protected task drive_read(input  bit   [31:0] addr,
                                     output logic [31:0] data);
this.vif.master_cb.paddr   <= addr;
     this.vif.master_cb.pwrite  <= '0;
     this.vif.master_cb.psel    <= '1;
     @ (this.vif.master_cb);
     this.vif.master_cb.penable <= '1;
     @ (this.vif.master_cb);
     data = this.vif.master_cb.prdata;
     this.vif.master_cb.psel    <= '0;
     this.vif.master_cb.penable <= '0;
     
   endtask: drive_read

   virtual protected task drive_write(input bit [31:0] addr,
                                input bit [31:0] data);
      this.vif.master_cb.paddr   <= addr;
      this.vif.master_cb.pwdata  <= data;
      this.vif.master_cb.pwrite  <= '1;
      this.vif.master_cb.psel    <= '1;
      @ (this.vif.master_cb);
      this.vif.master_cb.penable <= '1;
      @ (this.vif.master_cb);
      this.vif.master_cb.psel    <= '0;
      this.vif.master_cb.penable <= '0;
   endtask: drive_write

endclass: apb_master_drv

`endif//__APB_DRIVER__



                                                            
/////////////////////////////////// Monitor /////////////////////////////////
//-----------------------------------------
// APB Monitor class  
//-----------------------------------------

`ifndef __APB_MONITOR__
`define __APB_MONITOR__

class apb_monitor extends uvm_monitor;

  virtual apb_if vif;

  //Analysis port -parameterized to apb_rw transaction
  ///Monitor writes transaction objects to this port once detected on interface
  uvm_analysis_port#(apb_rw_seq_item) ap;

  //config class handle
  apb_config cfg;
  uvm_event  reset_done; 


  `uvm_component_utils(apb_monitor)

   function new(string name, uvm_component parent = null);
     super.new(name, parent);
     ap = new("ap", this);
   endfunction: new

   //Build Phase - Get handle to virtual if from agent/config_db
   virtual function void build_phase(uvm_phase phase);
     apb_agent agent;
     if ($cast(agent, get_parent()) && agent != null) begin
       vif = agent.vif;
     end
     else begin
       if (!uvm_config_db#(virtual apb_if)::get(this, "", "vif", vif)) begin
         `uvm_fatal("APB/MON/NOVIF", "No virtual interface specified for this monitor instance")
       end
     end
     
    if(!uvm_config_db#(uvm_event)::get( null, "*", "reset_done", reset_done))begin
	    `uvm_fatal(this.get_name(), "No reset_done specified for this test instance")
    end
   endfunction

   virtual task run_phase(uvm_phase phase);
     super.run_phase(phase);
     
     reset_done.wait_trigger();
     forever begin
       apb_rw_seq_item tr;
       // Wait for a SETUP cycle
         @ (negedge this.vif.pclk);
         
         if(!this.vif.prst && this.vif.psel && this.vif.penable) begin
         
	         //create a transaction object
	         tr = apb_rw_seq_item::type_id::create("tr", this);
        
	         //populate fields based on values seen on interface
	         tr.apb_cmd = (this.vif.pwrite) ? apb_rw_seq_item::WRITE : apb_rw_seq_item::READ;
	         tr.addr = this.vif.paddr;
         
         	 tr.data = (tr.apb_cmd == apb_rw_seq_item::READ) ? this.vif.prdata : this.vif.pwdata;
         
         
         	uvm_report_info("APB_MONITOR", $psprintf("Got Transaction %s",tr.convert2string()));
         	//Write to analysis port
         	ap.write(tr);
      	end
     end
   endtask: run_phase

endclass: apb_monitor

`endif //__APB_MONITOR__


////////////////////////////////////// Scoreboard //////////////////////////////
`ifndef __APB_SCOREBOARD__
`define __APB_SCOREBOARD__


class apb_scoreboard extends uvm_scoreboard;

	apb_rw_seq_item exp_que[$];
	
	//Register with factory for dynamic creation
    `uvm_component_utils(apb_scoreboard)

    
    //macro for define implementation port
	`uvm_analysis_imp_decl(_from_driver)
	`uvm_analysis_imp_decl(_from_monitor)

	//declaration of implementaion ports
    uvm_analysis_imp_from_driver  #(apb_rw_seq_item,apb_scoreboard) drv2Sb_port;
    uvm_analysis_imp_from_monitor #(apb_rw_seq_item,apb_scoreboard) mon2Sb_port;

    function new(string name, uvm_component parent);
        super.new(name, parent);
        
        //instantiation
        drv2Sb_port = new("drv2Sb_port", this);
        mon2Sb_port = new("mon2Sb_port", this);
    endfunction : new

    virtual function void write_drv2Sb_port(apb_rw_seq_item tr);
        exp_que.push_back(pkt);
   endfunction : write_drv2Sb_port

   virtual function void write_sent_pkt(input Packet pkt);
        if(exp_que.size()) begin
           exp_pkt = exp_que.pop_front();
           if( pkt.compare(exp_pkt))
             uvm_report_info(get_type_name(), $psprintf("Sent packet and received packet matched"), UVM_LOW);
           else
             uvm_report_error(get_type_name(), $psprintf("Sent packet and received packet mismatched"), UVM_LOW);
        end else begin
             uvm_report_error(get_type_name(), $psprintf("No more packets to in the expected queue to compare"), UVM_LOW);
   		end
   endfunction : write_sent_pkt


   virtual function void report();
        uvm_report_info(get_type_name(),
        $psprintf("Scoreboard Report \n", this.sprint()), UVM_LOW);
   endfunction : report


endclass : apb_scoreboard

`endif//__APB_SCOREBOARD__

//------------------------------------
// Basic APB  Read/Write Transaction class definition
//  This transaction will be used by Sequences, Drivers and Monitors
//------------------------------------
`ifndef __APB_RW_SEQ_ITEM__
`define __APB_RW_SEQ_ITEM__

import uvm_pkg::*;

//apb_rw sequence item derived from base uvm_sequence_item
class apb_rw_seq_item extends uvm_sequence_item;
 
   //typedef for READ/Write transaction type
   typedef enum {READ, WRITE} kind_e;
   rand bit   [31:0] addr;      //Address
   rand logic [31:0] data;     //Data - For write or read response
   rand kind_e  apb_cmd;       //command type

    //Register with factory for dynamic creation
   `uvm_object_utils(apb_rw_seq_item)
  
   
   function new (string name = "apb_rw_seq_item");
      super.new(name);
   endfunction

   function string convert2string();
     return $psprintf("kind=%s addr=%0h data=%0h ",apb_cmd,addr,data);
   endfunction

endclass: apb_rw_seq_item

`endif//__APB_RW_SEQ_ITEM__


///////////////////////////////////////////
//----------------------------------------------                                                        
// APB Env class                                                                                        
//----------------------------------------------

`ifndef __APB_ENV__
`define __APB_ENV__
                                                        
class apb_env  extends uvm_env;                                                                         
                                                                                                        
   `uvm_component_utils(apb_env);                                                                       
                                                                                                        
   //ENV class will have agent as its sub component                                                     
   apb_agent  agt;  
   apb_scoreboard scb;                                                                                     
   //virtual interface for APB interface                                                                
   virtual apb_if  vif;                                                                                 
                                                                                                        
   function new(string name, uvm_component parent = null);                                              
      super.new(name, parent);                                                                          
   endfunction                                                                                          
                                                                                                        
   //Build phase - Construct agent and get virtual interface handle from test  and pass it down to agent
   function void build_phase(uvm_phase phase);   
   	 super.build_phase(phase);   
   	    
     scb = apb_scoreboard::type_id::create("scb", this);                                                  
     agt = apb_agent::type_id::create("agt", this); 
                                                         
     if (!uvm_config_db#(virtual apb_if)::get(this, "", "vif", vif)) begin                              
         `uvm_fatal("APB/AGT/NOVIF", "No virtual interface specified for this env instance")            
     end 
     
     //pass it down to agent                                                                                               
     uvm_config_db#(virtual apb_if)::set( this, "agt", "vif", vif);                                     
   endfunction: build_phase  
   
   //Connect - driver and monitor analysis port to scoreboard                                            
   virtual function void connect_phase(uvm_phase phase);                                      
      agt.drv.Drvr2Sb_port.connect(scb.drv2Sb_port);                                         
      uvm_report_info("apb_agent::", "connect_phase, Connected driver to scb"); 
      
      agt.mon.ap.connect(scb.mon2Sb_port);                                         
      uvm_report_info("apb_agent::", "connect_phase, Connected monitor to scb");         
   endfunction                                                                             
                                                                                                        
endclass : apb_env                                                                                      
                                                                                                        
                                                                                                        
`endif  //__APB_ENV__                                                                                                