-- Seed: 1813498279121064682,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity iwrbz is
  port (fqo : linkage std_logic_vector(0 to 1); i : linkage bit; whchzic : buffer time; t : linkage std_logic_vector(3 downto 4));
end iwrbz;

architecture npr of iwrbz is
  
begin
  -- Single-driven assignments
  whchzic <= 2#1_0.0# us;
end npr;

library ieee;
use ieee.std_logic_1164.all;

entity lanei is
  port (xaaa : inout bit; zwetnd : inout std_logic; ocivwu : linkage integer; oifq : buffer bit_vector(2 downto 1));
end lanei;

library ieee;
use ieee.std_logic_1164.all;

architecture yyiuvzi of lanei is
  signal vfegbeuurl : std_logic_vector(3 downto 4);
  signal ahkh : time;
  signal yzmdzy : bit;
  signal zwrfeijur : std_logic_vector(0 to 1);
  signal s : std_logic_vector(3 downto 4);
  signal ybzvjaplil : time;
  signal xavbe : bit;
  signal io : std_logic_vector(0 to 1);
begin
  hlpemijg : entity work.iwrbz
    port map (fqo => io, i => xavbe, whchzic => ybzvjaplil, t => s);
  rxqihlxpgg : entity work.iwrbz
    port map (fqo => zwrfeijur, i => yzmdzy, whchzic => ahkh, t => vfegbeuurl);
  
  -- Single-driven assignments
  oifq <= ('0', '1');
  xaaa <= '0';
  
  -- Multi-driven assignments
  vfegbeuurl <= "";
  zwetnd <= 'L';
  zwetnd <= 'H';
  zwetnd <= 'H';
end yyiuvzi;

library ieee;
use ieee.std_logic_1164.all;

entity frkqtw is
  port (b : inout bit_vector(1 downto 2); wyskjuoe : buffer integer; kgitriywg : out std_logic);
end frkqtw;

library ieee;
use ieee.std_logic_1164.all;

architecture zg of frkqtw is
  signal engngj : std_logic_vector(3 downto 4);
  signal j : time;
  signal e : bit;
  signal rswy : std_logic_vector(0 to 1);
begin
  mrutrh : entity work.iwrbz
    port map (fqo => rswy, i => e, whchzic => j, t => engngj);
  
  -- Multi-driven assignments
  kgitriywg <= '0';
  rswy <= "11";
  kgitriywg <= 'L';
  kgitriywg <= 'U';
end zg;

library ieee;
use ieee.std_logic_1164.all;

entity hh is
  port ( xwljcsoopd : inout std_logic
  ; e : buffer std_logic_vector(2 downto 0)
  ; veddvlfcy : out time_vector(1 to 0)
  ; bu : buffer std_logic_vector(2 to 0)
  );
end hh;

library ieee;
use ieee.std_logic_1164.all;

architecture g of hh is
  signal yhhxartbgw : integer;
  signal n : bit_vector(1 downto 2);
  signal kdm : integer;
  signal jgwgnkhaf : bit_vector(1 downto 2);
  signal nne : time;
  signal yqeouxax : bit;
  signal rcmdlg : std_logic_vector(0 to 1);
  signal hrmhju : time;
  signal weryiec : bit;
  signal ffrcayshwm : std_logic_vector(0 to 1);
begin
  jwv : entity work.iwrbz
    port map (fqo => ffrcayshwm, i => weryiec, whchzic => hrmhju, t => bu);
  rpp : entity work.iwrbz
    port map (fqo => rcmdlg, i => yqeouxax, whchzic => nne, t => bu);
  rsfz : entity work.frkqtw
    port map (b => jgwgnkhaf, wyskjuoe => kdm, kgitriywg => xwljcsoopd);
  wojxiox : entity work.frkqtw
    port map (b => n, wyskjuoe => yhhxartbgw, kgitriywg => xwljcsoopd);
  
  -- Single-driven assignments
  veddvlfcy <= (others => 0 ns);
end g;



-- Seed after: 9928685938448090160,8421704836678237495
