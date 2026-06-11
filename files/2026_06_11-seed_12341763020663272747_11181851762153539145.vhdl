-- Seed: 12341763020663272747,11181851762153539145



entity tx is
  port (qtkjjyecr : in time; nquduw : buffer bit; hujyollgy : buffer time; rsqq : in integer);
end tx;



architecture um of tx is
  
begin
  
end um;

library ieee;
use ieee.std_logic_1164.all;

entity ehd is
  port (scxis : in std_logic; vqxuk : buffer std_logic; ou : linkage integer; nebvmu : buffer time);
end ehd;



architecture mks of ehd is
  signal o : bit;
  signal ku : integer;
  signal bftr : time;
  signal xhkb : bit;
  signal qrpmdj : time;
begin
  wowyxdqhg : entity work.tx
    port map (qtkjjyecr => qrpmdj, nquduw => xhkb, hujyollgy => bftr, rsqq => ku);
  pmum : entity work.tx
    port map (qtkjjyecr => nebvmu, nquduw => o, hujyollgy => qrpmdj, rsqq => ku);
end mks;

library ieee;
use ieee.std_logic_1164.all;

entity nxfc is
  port (gkc : in std_logic; jw : buffer std_logic; qllybqa : buffer time);
end nxfc;



architecture blrjnerxf of nxfc is
  signal evnsadoqgn : integer;
  signal k : time;
  signal jnvbnotful : bit;
  signal pus : time;
begin
  atlcwqkryw : entity work.tx
    port map (qtkjjyecr => pus, nquduw => jnvbnotful, hujyollgy => k, rsqq => evnsadoqgn);
end blrjnerxf;

library ieee;
use ieee.std_logic_1164.all;

entity ilpmjj is
  port (o : buffer boolean; hnkprk : buffer real; kq : in integer; yuht : out std_logic);
end ilpmjj;

library ieee;
use ieee.std_logic_1164.all;

architecture jdjspp of ilpmjj is
  signal eirhud : integer;
  signal v : bit;
  signal ylzqrp : time;
  signal bz : std_logic;
  signal aytnbadinb : time;
  signal se : bit;
  signal b : time;
  signal i : time;
begin
  gcj : entity work.nxfc
    port map (gkc => yuht, jw => yuht, qllybqa => i);
  ypbcdi : entity work.tx
    port map (qtkjjyecr => b, nquduw => se, hujyollgy => aytnbadinb, rsqq => kq);
  yhwpkijlx : entity work.nxfc
    port map (gkc => bz, jw => yuht, qllybqa => ylzqrp);
  c : entity work.tx
    port map (qtkjjyecr => ylzqrp, nquduw => v, hujyollgy => b, rsqq => eirhud);
end jdjspp;



-- Seed after: 16737994154165831336,11181851762153539145
