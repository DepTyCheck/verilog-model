-- Seed: 10137508955380255698,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity fg is
  port (kwqfovu : linkage string(4 downto 2); hd : linkage integer; taugns : inout std_logic);
end fg;

architecture bfgcz of fg is
  
begin
  -- Multi-driven assignments
  taugns <= taugns;
  taugns <= taugns;
  taugns <= '1';
end bfgcz;

entity ztlvsjtvc is
  port (gvwrddz : in time);
end ztlvsjtvc;

library ieee;
use ieee.std_logic_1164.all;

architecture zwgsckj of ztlvsjtvc is
  signal irqz : integer;
  signal vpjjvjcz : string(4 downto 2);
  signal uyfkwxvz : std_logic;
  signal sm : integer;
  signal ydbws : string(4 downto 2);
  signal uuwgojlub : std_logic;
  signal iv : integer;
  signal vwvlklftg : string(4 downto 2);
begin
  mvwjenov : entity work.fg
    port map (kwqfovu => vwvlklftg, hd => iv, taugns => uuwgojlub);
  uar : entity work.fg
    port map (kwqfovu => ydbws, hd => sm, taugns => uyfkwxvz);
  kdooapsog : entity work.fg
    port map (kwqfovu => vpjjvjcz, hd => irqz, taugns => uyfkwxvz);
  
  -- Multi-driven assignments
  uuwgojlub <= '0';
  uuwgojlub <= 'L';
  uuwgojlub <= 'X';
end zwgsckj;

entity sowzcazcj is
  port (mkygetod : out boolean);
end sowzcazcj;

library ieee;
use ieee.std_logic_1164.all;

architecture lf of sowzcazcj is
  signal ujwdfv : integer;
  signal ipvusf : string(4 downto 2);
  signal ppwymfzwn : std_logic;
  signal kz : integer;
  signal meixwo : string(4 downto 2);
  signal vjrxpeb : time;
begin
  nragejaw : entity work.ztlvsjtvc
    port map (gvwrddz => vjrxpeb);
  dee : entity work.ztlvsjtvc
    port map (gvwrddz => vjrxpeb);
  gnoztjtg : entity work.fg
    port map (kwqfovu => meixwo, hd => kz, taugns => ppwymfzwn);
  r : entity work.fg
    port map (kwqfovu => ipvusf, hd => ujwdfv, taugns => ppwymfzwn);
end lf;



-- Seed after: 2405927763731615227,11481034001933599325
