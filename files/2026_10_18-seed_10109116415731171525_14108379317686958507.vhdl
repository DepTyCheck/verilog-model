-- Seed: 10109116415731171525,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity zglqrurrx is
  port (qxb : buffer integer_vector(3 downto 2); o : inout time; flpo : buffer bit; bp : buffer std_logic_vector(2 to 4));
end zglqrurrx;

architecture si of zglqrurrx is
  
begin
  -- Single-driven assignments
  flpo <= flpo;
  qxb <= qxb;
  o <= o;
  
  -- Multi-driven assignments
  bp <= bp;
end si;

entity htmvnabu is
  port (vaqll : linkage integer; eqmlwyyx : out time);
end htmvnabu;

library ieee;
use ieee.std_logic_1164.all;

architecture uhokuusztq of htmvnabu is
  signal rjkj : std_logic_vector(2 to 4);
  signal iakno : bit;
  signal ukmtip : integer_vector(3 downto 2);
  signal mgyybdokqy : std_logic_vector(2 to 4);
  signal gnrmmcchwm : bit;
  signal dixoouvf : time;
  signal ncd : integer_vector(3 downto 2);
  signal u : std_logic_vector(2 to 4);
  signal sxbsgf : bit;
  signal seju : time;
  signal rmzastutr : integer_vector(3 downto 2);
  signal mnkyvauced : std_logic_vector(2 to 4);
  signal eyirqeblhe : bit;
  signal prlnpwlg : time;
  signal vr : integer_vector(3 downto 2);
begin
  mlyyp : entity work.zglqrurrx
    port map (qxb => vr, o => prlnpwlg, flpo => eyirqeblhe, bp => mnkyvauced);
  adulb : entity work.zglqrurrx
    port map (qxb => rmzastutr, o => seju, flpo => sxbsgf, bp => u);
  hjpskn : entity work.zglqrurrx
    port map (qxb => ncd, o => dixoouvf, flpo => gnrmmcchwm, bp => mgyybdokqy);
  j : entity work.zglqrurrx
    port map (qxb => ukmtip, o => eqmlwyyx, flpo => iakno, bp => rjkj);
  
  -- Multi-driven assignments
  mnkyvauced <= mnkyvauced;
  mnkyvauced <= mgyybdokqy;
  u <= ('0', 'L', 'Z');
end uhokuusztq;



-- Seed after: 12525239807068726669,14108379317686958507
