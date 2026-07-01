-- Seed: 6516311276453075943,6882842853887419669

entity r is
  port (hsmzztaje : inout time; yq : in bit_vector(4 downto 3); pog : inout boolean; frkbmr : inout real);
end r;

architecture ycp of r is
  
begin
  -- Single-driven assignments
  pog <= TRUE;
  hsmzztaje <= 1 hr;
  frkbmr <= 2#0.0#;
end ycp;

library ieee;
use ieee.std_logic_1164.all;

entity qe is
  port (ytexriimej : inout severity_level; nszhcd : inout real; dpuch : buffer std_logic; xbky : out integer);
end qe;

architecture oiubfi of qe is
  signal o : boolean;
  signal zkb : bit_vector(4 downto 3);
  signal xfbmeyeae : time;
  signal xvmdybzo : real;
  signal bjiiovl : boolean;
  signal pjxp : bit_vector(4 downto 3);
  signal ztqcb : time;
begin
  gdffq : entity work.r
    port map (hsmzztaje => ztqcb, yq => pjxp, pog => bjiiovl, frkbmr => xvmdybzo);
  uvpcaue : entity work.r
    port map (hsmzztaje => xfbmeyeae, yq => zkb, pog => o, frkbmr => nszhcd);
  
  -- Single-driven assignments
  xbky <= 4014;
  zkb <= ('0', '0');
  ytexriimej <= FAILURE;
  pjxp <= ('1', '1');
  
  -- Multi-driven assignments
  dpuch <= 'L';
end oiubfi;

library ieee;
use ieee.std_logic_1164.all;

entity aqyarrhks is
  port (dxnbjwkt : inout std_logic_vector(1 to 4); fxsqcblebd : out real; xopzcyujo : inout std_logic; qf : buffer boolean);
end aqyarrhks;

architecture uzulnqa of aqyarrhks is
  
begin
  -- Single-driven assignments
  qf <= FALSE;
  
  -- Multi-driven assignments
  dxnbjwkt <= ('Z', '0', 'U', 'Z');
  xopzcyujo <= '0';
  xopzcyujo <= 'H';
  xopzcyujo <= '-';
end uzulnqa;

library ieee;
use ieee.std_logic_1164.all;

entity knwqioxlxt is
  port (gkmlqhdjl : buffer integer; ubcebgz : in integer; yiqdbfcea : inout std_logic; imiye : buffer severity_level);
end knwqioxlxt;

library ieee;
use ieee.std_logic_1164.all;

architecture auyr of knwqioxlxt is
  signal mrclkx : real;
  signal xui : boolean;
  signal qqc : time;
  signal jtidrp : real;
  signal dlhqbkblg : boolean;
  signal o : bit_vector(4 downto 3);
  signal mgfv : time;
  signal shlauqg : integer;
  signal tabvuhr : std_logic;
  signal ixpbkzmhb : real;
  signal shumzg : severity_level;
  signal snvbnpay : boolean;
  signal v : real;
  signal vppzufj : std_logic_vector(1 to 4);
begin
  tiraix : entity work.aqyarrhks
    port map (dxnbjwkt => vppzufj, fxsqcblebd => v, xopzcyujo => yiqdbfcea, qf => snvbnpay);
  gowmcagodf : entity work.qe
    port map (ytexriimej => shumzg, nszhcd => ixpbkzmhb, dpuch => tabvuhr, xbky => shlauqg);
  w : entity work.r
    port map (hsmzztaje => mgfv, yq => o, pog => dlhqbkblg, frkbmr => jtidrp);
  zefktn : entity work.r
    port map (hsmzztaje => qqc, yq => o, pog => xui, frkbmr => mrclkx);
  
  -- Single-driven assignments
  imiye <= NOTE;
  o <= ('0', '1');
  gkmlqhdjl <= 8#0_1_5_7_4#;
  
  -- Multi-driven assignments
  tabvuhr <= 'Z';
  yiqdbfcea <= 'H';
  tabvuhr <= 'L';
end auyr;



-- Seed after: 14551810022022310270,6882842853887419669
