-- Seed: 4701984679382700991,11460338385561364835

library ieee;
use ieee.std_logic_1164.all;

entity baandqso is
  port (bmwucs : linkage std_logic; scgobf : out integer);
end baandqso;



architecture e of baandqso is
  
begin
  
end e;



entity bsnm is
  port (evrzs : in real; qcuvw : out time);
end bsnm;

library ieee;
use ieee.std_logic_1164.all;

architecture ahtqicsgyp of bsnm is
  signal bupo : integer;
  signal qqyclsjssl : integer;
  signal xmpuci : std_logic;
  signal fztsn : integer;
  signal yd : std_logic;
begin
  vxw : entity work.baandqso
    port map (bmwucs => yd, scgobf => fztsn);
  e : entity work.baandqso
    port map (bmwucs => xmpuci, scgobf => qqyclsjssl);
  rogdbyrv : entity work.baandqso
    port map (bmwucs => yd, scgobf => bupo);
end ahtqicsgyp;



entity uut is
  port (e : linkage real; a : inout integer);
end uut;

library ieee;
use ieee.std_logic_1164.all;

architecture oyxtqjyw of uut is
  signal euesklinfc : std_logic;
begin
  jpoalyph : entity work.baandqso
    port map (bmwucs => euesklinfc, scgobf => a);
end oyxtqjyw;



-- Seed after: 9576829740413425612,11460338385561364835
