-- Seed: 14829856383053947929,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity rrezccllg is
  port (ev : in std_logic; xh : in severity_level);
end rrezccllg;

architecture shp of rrezccllg is
  
begin
  
end shp;

entity scbxozsqdp is
  port (y : in time; s : buffer integer; lithqyasr : buffer real);
end scbxozsqdp;

library ieee;
use ieee.std_logic_1164.all;

architecture tuqyomwhrw of scbxozsqdp is
  signal tyfcidk : std_logic;
  signal riga : severity_level;
  signal okjftpe : std_logic;
begin
  xlbeymselz : entity work.rrezccllg
    port map (ev => okjftpe, xh => riga);
  nfhzd : entity work.rrezccllg
    port map (ev => tyfcidk, xh => riga);
end tuqyomwhrw;

entity dai is
  port (wcrbhh : out real);
end dai;

library ieee;
use ieee.std_logic_1164.all;

architecture oivxamzdh of dai is
  signal b : integer;
  signal ymitoomlus : severity_level;
  signal ytrfsad : real;
  signal vwoigk : integer;
  signal lht : time;
  signal fpuohxh : severity_level;
  signal xcw : std_logic;
begin
  eoyx : entity work.rrezccllg
    port map (ev => xcw, xh => fpuohxh);
  vkjniwjac : entity work.scbxozsqdp
    port map (y => lht, s => vwoigk, lithqyasr => ytrfsad);
  ncphevfyuh : entity work.rrezccllg
    port map (ev => xcw, xh => ymitoomlus);
  xdmujpos : entity work.scbxozsqdp
    port map (y => lht, s => b, lithqyasr => wcrbhh);
  
  -- Single-driven assignments
  lht <= lht;
  
  -- Multi-driven assignments
  xcw <= '1';
  xcw <= xcw;
  xcw <= xcw;
  xcw <= xcw;
end oivxamzdh;



-- Seed after: 6278502215180444556,11127274767545411571
