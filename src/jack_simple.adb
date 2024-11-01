with Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with System;

procedure Jack_Simple is
   package IO renames Ada.Text_IO;
   package C renames Interfaces.C;
   package CStr renames Interfaces.C.Strings;
   use C;
   use CStr;

   package CStr_Array is
      new Interfaces.C.Pointers (C.size_t, CStr.chars_ptr, CStr.chars_ptr_array, CStr.null_ptr);

   type jack_client_t is new System.Address;
   type jack_client_t_access is access jack_client_t;
   type jack_port_t is new System.Address;
   type jack_port_t_access is access jack_port_t;
   type jack_nframes is mod 2 ** 31;

   CFloat_Array_Terminator : constant Float := 0.0;
   type CFloat_Array is array (jack_nframes range <>) of aliased Float;
   package CFloat_Ptr is
      new Interfaces.C.Pointers (jack_nframes, Float, CFloat_Array, CFloat_Array_Terminator);
   
   type Jack_Callback_Data is record
      Client : jack_client_t_access := null;
      Output_Port : jack_port_t_access := null;
   end record;

   type jack_process_callback is access function (nframes : jack_nframes; arg : Jack_Callback_Data) return C.int
      with Convention => C;
         
   type jack_options_t is (
      jack_null_option,
      jack_no_start_server,
      jack_use_exact_name,
      jack_server_name,
      jack_load_name,
      jack_load_init,
      jack_session_id
   ) with Convention => C;
   for jack_options_t use (
      jack_null_option       => 16#00#,
      jack_no_start_server   => 16#01#,
      jack_use_exact_name    => 16#02#,
      jack_server_name       => 16#04#,
      jack_load_name         => 16#08#,
      jack_load_init         => 16#10#,
      jack_session_id        => 16#20#
   );

   type jack_status_t is (
      jack_failure,
      jack_invalid_option,
      jack_name_not_unique,
      jack_server_started,
      jack_server_failed,
      jack_server_error,
      jack_no_such_client,
      jack_load_failure,
      jack_init_failure,
      jack_shm_failure,
      jack_version_error,
      jack_backend_error,
      jack_client_zombie
   ) with Convention => C;
   for jack_status_t use (
      jack_failure           => 16#01#,
      jack_invalid_option    => 16#02#,
      jack_name_not_unique   => 16#04#,
      jack_server_started    => 16#08#,
      jack_server_failed     => 16#10#,
      jack_server_error      => 16#20#,
      jack_no_such_client    => 16#40#,
      jack_load_failure      => 16#80#,
      jack_init_failure      => 16#100#,
      jack_shm_failure       => 16#200#,
      jack_version_error     => 16#400#,
      jack_backend_error     => 16#800#,
      jack_client_zombie     => 16#10000#
   );

   type jack_port_flags is (
      jack_port_is_input,
      jack_port_is_output,
      jack_port_is_physical,
      jack_port_can_monitor,
      jack_port_is_terminal
   ) with Convention => C;
   for jack_port_flags use (
      jack_port_is_input     => 16#01#,
      jack_port_is_output    => 16#02#,
      jack_port_is_physical  => 16#04#,
      jack_port_can_monitor  => 16#08#,
      jack_port_is_terminal  => 16#10#
   );

   function jack_client_open (Client_Name : C.char_array; Options: jack_options_t; Status: access jack_status_t) return jack_client_t_access
      with Import, Convention => C, External_Name => "jack_client_open";
   function jack_client_close (Client : jack_client_t_access) return C.int
      with Import, Convention => C, External_Name => "jack_client_close";

   function jack_activate (Client : jack_client_t_access) return C.int
      with Import, Convention => C, External_Name => "jack_activate";
   function jack_deactivate (Client : jack_client_t_access) return C.int
      with Import, Convention => C, External_Name => "jack_deactivate";

   function jack_set_process_callback (Client : jack_client_t_access; Callback : jack_process_callback; arg : Jack_Callback_Data) return C.int
      with Import, Convention => C, External_Name => "jack_set_process_callback";

   function jack_get_sample_rate (Client : jack_client_t_access) return jack_nframes
      with Import, Convention => C, External_Name => "jack_get_sample_rate";
   function jack_get_buffer_size (Client : jack_client_t_access) return jack_nframes
      with Import, Convention => C, External_Name => "jack_get_buffer_size";

   function jack_get_ports (Client : jack_client_t_access; Port_Name_Pattern, Type_Name_Pattern : C.char_array; Flags : C.unsigned) return CStr_Array.Pointer
     with Import, Convention => C, External_Name => "jack_get_ports";

   function jack_port_register (Client : jack_client_t_access; Port_Name, Port_Type : C.char_array; Flags, Buffer_Size : C.unsigned) return jack_port_t_access
      with Import, Convention => C, External_Name => "jack_port_register";
   function jack_port_name (Port : jack_port_t_access) return CStr.chars_ptr
      with Import, Convention => C, External_Name => "jack_port_name";
   function jack_port_name_size return C.int
      with Import, Convention => C, External_Name => "jack_port_name_size";
   function jack_port_get_buffer (Port : jack_port_t_access; nframes : jack_nframes) return CFloat_Ptr.Pointer
       with Import, Convention => C, External_Name => "jack_port_get_buffer";

   function jack_connect (Client : jack_client_t_access; Source_Port, Destination_Port : C.char_array) return C.int
      with Import, Convention => C, External_Name => "jack_connect";
   function jack_disconnect (Client : jack_client_t_access; Source_Port, Destination_Port : C.char_array) return C.int
      with Import, Convention => C, External_Name => "jack_disconnect";

   -- TODO: Maybe there is a better way?
   -- void jack_free (void* ptr);
   procedure jack_free (ptr : System.Address) 
      with Import, Convention => C, External_Name => "jack_free";
   procedure jack_free (ptr : CStr_Array.Pointer) 
      with Import, Convention => C, External_Name => "jack_free";

   function Audio_Process (nframes : jack_nframes; arg : Jack_Callback_Data) return C.int
      with Convention => C;

   Jack_Client_Failed_To_Open : exception;
   Jack_Client_Failed_To_Close : exception;
   Jack_Port_Failed_To_Open : exception;
   Jack_Failed_To_Set_Process_Callback : exception;
   
   JACK_DEFAULT_AUDIO_TYPE : constant C.char_array := "32 bit float mono audio" & C.nul;

   PI : constant Float := 3.1415926535;

   Name : constant C.char_array := "Jack Simple" & C.nul;
   Client : jack_client_t_access := null;
   Ports : CStr_Array.Pointer := null;
   Port_O : jack_port_t_access := null;
   Port_Flags : C.unsigned := jack_port_is_physical'Enum_Rep or jack_port_is_input'Enum_Rep;

   Phase : Float := 0.0;
   function Audio_Process (nframes : jack_nframes; arg : Jack_Callback_Data) return C.int is
      Output : CFloat_Ptr.Pointer := jack_port_get_buffer (arg.Output_Port, nframes);
      
      Sample_Rate : constant Float := Float (jack_get_sample_rate (arg.Client));
      Frequency : constant Float := (280.0 / Sample_Rate);
      Amplitude : constant Float := 0.1;
   begin
      for I in 0 .. nframes - 1 loop
         Phase := @ + Frequency;
         if Phase > 1.0 then Phase := @ - 1.0; end if;

         Output.all := Amplitude * Sin (Phase * 2.0 * PI);
         CFloat_Ptr.Increment (Output);
      end loop;
      return 0;
   end Audio_Process;
begin
   Client := jack_client_open (Name, jack_null_option, null);
   if Client = null then raise Jack_Client_Failed_To_Open; end if;
   Port_O := jack_port_register (Client, "Output" & C.nul, JACK_DEFAULT_AUDIO_TYPE, C.unsigned (jack_port_is_output'Enum_Rep), 0);
   if Port_O = null then raise Jack_Port_Failed_To_Open; end if;

   if jack_set_process_callback (Client, Audio_Process'Access, (Client => Client, Output_Port => Port_O)) /= 0 then 
      raise Jack_Failed_To_Set_Process_Callback; 
   end if;

   IO.Put_Line ("jack_get_sample_rate:" & jack_get_sample_rate(Client)'Image);
   IO.Put_Line ("jack_get_buffer_size:" & jack_get_buffer_size(Client)'Image);
   IO.Put_Line ("jack_activate:" & jack_activate(Client)'Image);

   Ports := jack_get_ports (Client, C.nul & C.nul, C.nul & C.nul, jack_port_is_physical'Enum_Rep or jack_port_is_input'Enum_Rep);
   declare 
      Port_Strs : constant CStr.chars_ptr_array := CStr_Array.Value (Ports);
      Port_Strs_Len : constant C.size_t := C.size_t (CStr_Array.Virtual_Length (Ports));

      SN : constant C.char_array := CStr.Value (jack_port_name (Port_O));
   begin
      for I in 0 .. Port_Strs_Len - 1 loop
         IO.Put_Line ("jack_connect " & I'Image & ":" & 
            jack_connect (Client, SN, CStr.Value (Port_Strs (I)))'Image);
      end loop;
   end;
   jack_free (Ports);

   delay 5.0;

   IO.Put_Line ("jack_deactivate:" & jack_deactivate(Client)'Image);
   if jack_client_close(Client) /= 0 then raise Jack_Client_Failed_To_Close; end if;
end Jack_Simple;
