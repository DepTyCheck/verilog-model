-- Seed: 6492425597685919899,17219977430298397867

entity kd is
  port (ezba : out time; kds : buffer time_vector(4 to 2); vefup : buffer bit; vcbiujt : inout time);
end kd;

architecture hrvejnpbpe of kd is
  
begin
  -- Single-driven assignments
  vcbiujt <= 4_3 ms;
  vefup <= vefup;
  ezba <= 2#100# fs;
end hrvejnpbpe;

library ieee;
use ieee.std_logic_1164.all;

entity twxsgy is
  port (qjc : linkage std_logic_vector(0 to 0));
end twxsgy;

architecture nkzg of twxsgy is
  signal vxsevyo : time;
  signal tbgooegu : bit;
  signal adc : time_vector(4 to 2);
  signal aiwfm : time;
begin
  aaez : entity work.kd
    port map (ezba => aiwfm, kds => adc, vefup => tbgooegu, vcbiujt => vxsevyo);
end nkzg;

entity bptjrle is
  port (fqkhllunos : out real; lahc : inout real_vector(0 downto 2));
end bptjrle;

library ieee;
use ieee.std_logic_1164.all;

architecture t of bptjrle is
  signal msjtdzhhv : std_logic_vector(0 to 0);
begin
  pcwejqd : entity work.twxsgy
    port map (qjc => msjtdzhhv);
  vtxv : entity work.twxsgy
    port map (qjc => msjtdzhhv);
  
  -- Multi-driven assignments
  msjtdzhhv <= "1";
end t;



-- Seed after: 17545045021520356695,17219977430298397867
