-- Seed: 4632568171073207543,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (hbxmhsqpb : buffer std_logic_vector(0 to 4); jvmwyvoxys : linkage integer; wozpuwdxqa : in time);
end x;

architecture trriawx of x is
  
begin
  -- Multi-driven assignments
  hbxmhsqpb <= "X0U-U";
  hbxmhsqpb <= hbxmhsqpb;
  hbxmhsqpb <= hbxmhsqpb;
  hbxmhsqpb <= ('0', 'L', 'L', 'X', 'X');
end trriawx;

entity jbhuxlbblb is
  port (vsmgaqkxwl : inout bit; kanipgz : inout boolean; medlgx : inout real; jp : out real);
end jbhuxlbblb;

library ieee;
use ieee.std_logic_1164.all;

architecture amxag of jbhuxlbblb is
  signal cka : time;
  signal ybu : integer;
  signal jatbkv : std_logic_vector(0 to 4);
  signal rvvquwb : time;
  signal ve : integer;
  signal thft : time;
  signal merrpweu : integer;
  signal luhvdu : std_logic_vector(0 to 4);
begin
  aoytx : entity work.x
    port map (hbxmhsqpb => luhvdu, jvmwyvoxys => merrpweu, wozpuwdxqa => thft);
  s : entity work.x
    port map (hbxmhsqpb => luhvdu, jvmwyvoxys => ve, wozpuwdxqa => rvvquwb);
  yoan : entity work.x
    port map (hbxmhsqpb => jatbkv, jvmwyvoxys => ybu, wozpuwdxqa => cka);
  
  -- Single-driven assignments
  jp <= 16#0_8_A_F_5.B2A#;
  medlgx <= 16#8.C_F_8#;
  kanipgz <= kanipgz;
  
  -- Multi-driven assignments
  luhvdu <= luhvdu;
  luhvdu <= luhvdu;
  luhvdu <= luhvdu;
end amxag;



-- Seed after: 11161444368093961955,8412319452373742525
