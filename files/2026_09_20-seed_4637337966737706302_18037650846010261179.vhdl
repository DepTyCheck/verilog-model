-- Seed: 4637337966737706302,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity amimasgaik is
  port (cpoo : in std_logic_vector(0 to 4); cec : linkage std_logic; tgvvczmez : inout std_logic_vector(4 to 1); tnugky : inout boolean);
end amimasgaik;

architecture hixvgnon of amimasgaik is
  
begin
  -- Single-driven assignments
  tnugky <= FALSE;
  
  -- Multi-driven assignments
  tgvvczmez <= tgvvczmez;
  tgvvczmez <= "";
end hixvgnon;

library ieee;
use ieee.std_logic_1164.all;

entity we is
  port (cnubxe : buffer severity_level; ej : in std_logic);
end we;

library ieee;
use ieee.std_logic_1164.all;

architecture ojubcessng of we is
  signal ou : boolean;
  signal gqbwlfr : std_logic_vector(4 to 1);
  signal szdp : boolean;
  signal kv : std_logic_vector(4 to 1);
  signal vugjnnap : boolean;
  signal hsiajhdwdn : std_logic_vector(4 to 1);
  signal avdcxpgq : std_logic_vector(0 to 4);
begin
  esucbnqt : entity work.amimasgaik
    port map (cpoo => avdcxpgq, cec => ej, tgvvczmez => hsiajhdwdn, tnugky => vugjnnap);
  jwmarzom : entity work.amimasgaik
    port map (cpoo => avdcxpgq, cec => ej, tgvvczmez => kv, tnugky => szdp);
  rhtx : entity work.amimasgaik
    port map (cpoo => avdcxpgq, cec => ej, tgvvczmez => gqbwlfr, tnugky => ou);
  
  -- Single-driven assignments
  cnubxe <= FAILURE;
  
  -- Multi-driven assignments
  avdcxpgq <= ('0', 'Z', 'U', '-', 'W');
  avdcxpgq <= "X0UX1";
  avdcxpgq <= ('-', '0', 'U', 'U', '0');
end ojubcessng;

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (hyv : inout std_logic);
end l;

library ieee;
use ieee.std_logic_1164.all;

architecture ybunet of l is
  signal smskcyn : boolean;
  signal mq : std_logic_vector(4 to 1);
  signal ywciey : std_logic;
  signal ocnxqojeiu : severity_level;
  signal mct : boolean;
  signal xqxnldfujo : std_logic;
  signal hp : boolean;
  signal yoelp : std_logic_vector(4 to 1);
  signal gfgnmg : std_logic;
  signal hfucdjs : std_logic_vector(0 to 4);
begin
  cmzhxf : entity work.amimasgaik
    port map (cpoo => hfucdjs, cec => gfgnmg, tgvvczmez => yoelp, tnugky => hp);
  gz : entity work.amimasgaik
    port map (cpoo => hfucdjs, cec => xqxnldfujo, tgvvczmez => yoelp, tnugky => mct);
  txswqtyokd : entity work.we
    port map (cnubxe => ocnxqojeiu, ej => ywciey);
  dphzbejheq : entity work.amimasgaik
    port map (cpoo => hfucdjs, cec => hyv, tgvvczmez => mq, tnugky => smskcyn);
  
  -- Multi-driven assignments
  hyv <= 'U';
  gfgnmg <= hyv;
  gfgnmg <= 'H';
  yoelp <= (others => '0');
end ybunet;



-- Seed after: 10481069420909474851,18037650846010261179
