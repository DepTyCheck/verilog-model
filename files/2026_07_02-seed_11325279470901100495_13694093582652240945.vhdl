-- Seed: 11325279470901100495,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity oup is
  port (rkgysxyrg : out real; eshtolnxm : linkage std_logic; f : inout character);
end oup;

architecture yokwuqwrd of oup is
  
begin
  -- Single-driven assignments
  f <= 'q';
  rkgysxyrg <= 16#7E2E.558#;
end yokwuqwrd;

library ieee;
use ieee.std_logic_1164.all;

entity dyxlyj is
  port (oisqddlsjf : buffer integer_vector(0 to 0); wufpcrdp : linkage std_logic; wmm : inout integer_vector(4 to 1));
end dyxlyj;

architecture knqqka of dyxlyj is
  signal jvc : character;
  signal hryawei : real;
  signal uv : character;
  signal kg : real;
begin
  orkb : entity work.oup
    port map (rkgysxyrg => kg, eshtolnxm => wufpcrdp, f => uv);
  hezjxq : entity work.oup
    port map (rkgysxyrg => hryawei, eshtolnxm => wufpcrdp, f => jvc);
  
  -- Single-driven assignments
  wmm <= (others => 0);
end knqqka;

entity igh is
  port (xsvrb : buffer bit_vector(3 to 0); xzrqwscf : buffer integer);
end igh;

library ieee;
use ieee.std_logic_1164.all;

architecture jasyxvtey of igh is
  signal alovmtswha : character;
  signal kh : real;
  signal zjj : character;
  signal ixnhphopt : std_logic;
  signal kfjutcjd : real;
  signal cgleycpq : integer_vector(4 to 1);
  signal ezjrekjlpx : std_logic;
  signal feveiqtfc : integer_vector(0 to 0);
  signal npmfndvi : character;
  signal mmsgow : std_logic;
  signal pqotqgyal : real;
begin
  y : entity work.oup
    port map (rkgysxyrg => pqotqgyal, eshtolnxm => mmsgow, f => npmfndvi);
  n : entity work.dyxlyj
    port map (oisqddlsjf => feveiqtfc, wufpcrdp => ezjrekjlpx, wmm => cgleycpq);
  jcuwanb : entity work.oup
    port map (rkgysxyrg => kfjutcjd, eshtolnxm => ixnhphopt, f => zjj);
  yxdlwso : entity work.oup
    port map (rkgysxyrg => kh, eshtolnxm => ixnhphopt, f => alovmtswha);
  
  -- Multi-driven assignments
  ixnhphopt <= 'L';
end jasyxvtey;



-- Seed after: 11113504799662172288,13694093582652240945
