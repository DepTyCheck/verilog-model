-- Seed: 5659881340591366071,5444032936641997477

entity dlrmhwz is
  port (ng : inout boolean; vlazjjb : buffer time);
end dlrmhwz;

architecture ilhtbazy of dlrmhwz is
  
begin
  
end ilhtbazy;

entity igew is
  port (ibvvcooo : out time);
end igew;

architecture ltty of igew is
  signal ptxjkey : time;
  signal yahtqvjr : boolean;
  signal m : boolean;
  signal wxvzdmkvtc : time;
  signal q : boolean;
  signal ycdwwxi : time;
  signal eiqwfal : boolean;
begin
  wtfmoxp : entity work.dlrmhwz
    port map (ng => eiqwfal, vlazjjb => ycdwwxi);
  rxtdrt : entity work.dlrmhwz
    port map (ng => q, vlazjjb => wxvzdmkvtc);
  znl : entity work.dlrmhwz
    port map (ng => m, vlazjjb => ibvvcooo);
  wolpulyzb : entity work.dlrmhwz
    port map (ng => yahtqvjr, vlazjjb => ptxjkey);
end ltty;

library ieee;
use ieee.std_logic_1164.all;

entity jzth is
  port (lrfz : out real; mtzyr : out std_logic_vector(0 to 0); byyi : buffer time; c : in integer_vector(0 to 1));
end jzth;

architecture opnlvtdfjd of jzth is
  signal hrb : time;
  signal qaf : boolean;
  signal xvf : time;
begin
  pbvjhs : entity work.igew
    port map (ibvvcooo => xvf);
  ioraebtm : entity work.dlrmhwz
    port map (ng => qaf, vlazjjb => hrb);
  
  -- Single-driven assignments
  byyi <= xvf;
  lrfz <= 2#1_1_0.1_0_1#;
  
  -- Multi-driven assignments
  mtzyr <= mtzyr;
end opnlvtdfjd;



-- Seed after: 18316501862579239210,5444032936641997477
