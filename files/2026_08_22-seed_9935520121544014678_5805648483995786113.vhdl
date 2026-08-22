-- Seed: 9935520121544014678,5805648483995786113

entity stldynzre is
  port (ryknsbyrl : inout time);
end stldynzre;

architecture dyhaymhff of stldynzre is
  
begin
  -- Single-driven assignments
  ryknsbyrl <= 0_0_0 ps;
end dyhaymhff;

entity djxq is
  port (cktm : buffer integer; bnp : buffer real);
end djxq;

architecture yy of djxq is
  signal khqzxed : time;
  signal abffzqxd : time;
begin
  tcevijr : entity work.stldynzre
    port map (ryknsbyrl => abffzqxd);
  ewqxdfhw : entity work.stldynzre
    port map (ryknsbyrl => khqzxed);
end yy;

library ieee;
use ieee.std_logic_1164.all;

entity ngsebytlo is
  port (uaozlin : in std_logic_vector(4 to 3); xjk : linkage time);
end ngsebytlo;

architecture mi of ngsebytlo is
  signal lapiulbtyi : time;
  signal f : time;
begin
  maewqm : entity work.stldynzre
    port map (ryknsbyrl => f);
  ym : entity work.stldynzre
    port map (ryknsbyrl => lapiulbtyi);
end mi;

library ieee;
use ieee.std_logic_1164.all;

entity uc is
  port (edxucaoasb : inout time; sk : linkage std_logic_vector(4 downto 4); asib : inout std_logic);
end uc;

library ieee;
use ieee.std_logic_1164.all;

architecture jttyqvxr of uc is
  signal akbn : time;
  signal r : time;
  signal ootetlrnc : time;
  signal lvvptibz : std_logic_vector(4 to 3);
begin
  pkaohjvqur : entity work.ngsebytlo
    port map (uaozlin => lvvptibz, xjk => ootetlrnc);
  gagr : entity work.ngsebytlo
    port map (uaozlin => lvvptibz, xjk => r);
  yvayfq : entity work.stldynzre
    port map (ryknsbyrl => akbn);
  zwjgboxjv : entity work.stldynzre
    port map (ryknsbyrl => edxucaoasb);
  
  -- Multi-driven assignments
  asib <= 'L';
end jttyqvxr;



-- Seed after: 15014527396100667051,5805648483995786113
