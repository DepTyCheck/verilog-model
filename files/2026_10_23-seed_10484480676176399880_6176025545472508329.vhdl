-- Seed: 10484480676176399880,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity zcnrex is
  port (bakn : in time; wsivjvq : in std_logic; yroujiooli : buffer std_logic; wstpk : buffer string(4 to 5));
end zcnrex;

architecture cuoo of zcnrex is
  
begin
  -- Single-driven assignments
  wstpk <= wstpk;
  
  -- Multi-driven assignments
  yroujiooli <= '-';
  yroujiooli <= wsivjvq;
  yroujiooli <= 'L';
  yroujiooli <= yroujiooli;
end cuoo;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (kblqotye : linkage std_logic);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture gkhhp of w is
  signal iwdpzk : string(4 to 5);
  signal qgux : std_logic;
  signal n : time;
  signal olvykzwx : string(4 to 5);
  signal df : std_logic;
  signal ozxp : string(4 to 5);
  signal hstghjb : std_logic;
  signal aipqibbj : std_logic;
  signal vt : string(4 to 5);
  signal onr : std_logic;
  signal ddlqpkfagd : std_logic;
  signal baygmk : time;
begin
  dqryfmtc : entity work.zcnrex
    port map (bakn => baygmk, wsivjvq => ddlqpkfagd, yroujiooli => onr, wstpk => vt);
  ffumlep : entity work.zcnrex
    port map (bakn => baygmk, wsivjvq => aipqibbj, yroujiooli => hstghjb, wstpk => ozxp);
  teczgbpuzf : entity work.zcnrex
    port map (bakn => baygmk, wsivjvq => df, yroujiooli => ddlqpkfagd, wstpk => olvykzwx);
  kdssrprd : entity work.zcnrex
    port map (bakn => n, wsivjvq => qgux, yroujiooli => ddlqpkfagd, wstpk => iwdpzk);
  
  -- Single-driven assignments
  baygmk <= n;
  
  -- Multi-driven assignments
  aipqibbj <= '0';
  ddlqpkfagd <= 'H';
end gkhhp;

entity tvjnxn is
  port (obkzbm : out integer);
end tvjnxn;

library ieee;
use ieee.std_logic_1164.all;

architecture suesr of tvjnxn is
  signal kvvomi : std_logic;
begin
  qwgoktzk : entity work.w
    port map (kblqotye => kvvomi);
  
  -- Single-driven assignments
  obkzbm <= obkzbm;
  
  -- Multi-driven assignments
  kvvomi <= '1';
end suesr;

library ieee;
use ieee.std_logic_1164.all;

entity bjzi is
  port (mbfdknxsjm : inout std_logic_vector(0 downto 3); arrd : linkage time);
end bjzi;

library ieee;
use ieee.std_logic_1164.all;

architecture fbsnze of bjzi is
  signal jzmeikxys : integer;
  signal ivgvfw : string(4 to 5);
  signal tufhw : std_logic;
  signal sjzkry : std_logic;
  signal mdu : time;
begin
  kccrihvy : entity work.zcnrex
    port map (bakn => mdu, wsivjvq => sjzkry, yroujiooli => tufhw, wstpk => ivgvfw);
  gajmcj : entity work.tvjnxn
    port map (obkzbm => jzmeikxys);
end fbsnze;



-- Seed after: 6129327537201670050,6176025545472508329
