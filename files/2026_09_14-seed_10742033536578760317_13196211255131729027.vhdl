-- Seed: 10742033536578760317,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity sdeyu is
  port (einrwm : inout real_vector(4 downto 3); yfiwmbdlap : out std_logic_vector(0 downto 2); qiimpt : out std_logic_vector(0 to 3));
end sdeyu;

architecture xg of sdeyu is
  
begin
  -- Single-driven assignments
  einrwm <= (2#0_0_1_0_0.11000#, 343.1300);
end xg;

library ieee;
use ieee.std_logic_1164.all;

entity wflcya is
  port (fmbowxfaqt : inout boolean_vector(4 to 3); cxpvjwkkg : in std_logic);
end wflcya;

library ieee;
use ieee.std_logic_1164.all;

architecture coqiibcmod of wflcya is
  signal wzgcjlg : std_logic_vector(0 to 3);
  signal k : std_logic_vector(0 downto 2);
  signal guqmqyzsgk : real_vector(4 downto 3);
begin
  aozhorzhc : entity work.sdeyu
    port map (einrwm => guqmqyzsgk, yfiwmbdlap => k, qiimpt => wzgcjlg);
  
  -- Single-driven assignments
  fmbowxfaqt <= fmbowxfaqt;
end coqiibcmod;

entity crupapf is
  port (luju : out integer);
end crupapf;

library ieee;
use ieee.std_logic_1164.all;

architecture cirs of crupapf is
  signal uetmrisqv : real_vector(4 downto 3);
  signal fet : std_logic_vector(0 to 3);
  signal vesgik : real_vector(4 downto 3);
  signal tjp : std_logic_vector(0 to 3);
  signal enjqglrh : std_logic_vector(0 downto 2);
  signal y : real_vector(4 downto 3);
begin
  xsbspgoxtk : entity work.sdeyu
    port map (einrwm => y, yfiwmbdlap => enjqglrh, qiimpt => tjp);
  p : entity work.sdeyu
    port map (einrwm => vesgik, yfiwmbdlap => enjqglrh, qiimpt => fet);
  x : entity work.sdeyu
    port map (einrwm => uetmrisqv, yfiwmbdlap => enjqglrh, qiimpt => tjp);
  
  -- Single-driven assignments
  luju <= 2#0_0#;
end cirs;

library ieee;
use ieee.std_logic_1164.all;

entity tejy is
  port (k : linkage integer; ut : linkage std_logic_vector(0 downto 4));
end tejy;

library ieee;
use ieee.std_logic_1164.all;

architecture rcklru of tejy is
  signal zyts : std_logic_vector(0 to 3);
  signal tfvvtryqdy : std_logic_vector(0 downto 2);
  signal umoic : real_vector(4 downto 3);
  signal o : integer;
  signal fntaqfjsyn : std_logic;
  signal zyqpbib : boolean_vector(4 to 3);
  signal gbuavomqh : std_logic_vector(0 to 3);
  signal jy : std_logic_vector(0 downto 2);
  signal ebyhwvtmri : real_vector(4 downto 3);
begin
  x : entity work.sdeyu
    port map (einrwm => ebyhwvtmri, yfiwmbdlap => jy, qiimpt => gbuavomqh);
  cz : entity work.wflcya
    port map (fmbowxfaqt => zyqpbib, cxpvjwkkg => fntaqfjsyn);
  uaqjbfrflm : entity work.crupapf
    port map (luju => o);
  tyw : entity work.sdeyu
    port map (einrwm => umoic, yfiwmbdlap => tfvvtryqdy, qiimpt => zyts);
  
  -- Multi-driven assignments
  jy <= jy;
  zyts <= gbuavomqh;
end rcklru;



-- Seed after: 15034762729096208723,13196211255131729027
