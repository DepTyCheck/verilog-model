-- Seed: 870553214920894653,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity bpo is
  port ( crcxrapian : in integer_vector(0 downto 0)
  ; wvop : inout std_logic_vector(1 to 3)
  ; gadefhkey : linkage integer
  ; dg : out std_logic_vector(4 downto 0)
  );
end bpo;

architecture zf of bpo is
  
begin
  
end zf;

library ieee;
use ieee.std_logic_1164.all;

entity bklbvd is
  port (r : buffer std_logic; hazu : buffer std_logic_vector(3 downto 1); v : buffer std_logic_vector(0 to 4); pfudrwvx : inout severity_level);
end bklbvd;

library ieee;
use ieee.std_logic_1164.all;

architecture ble of bklbvd is
  signal i : integer;
  signal utcz : std_logic_vector(1 to 3);
  signal fectrw : integer;
  signal yfsu : integer_vector(0 downto 0);
  signal nkmtuxojug : std_logic_vector(4 downto 0);
  signal lu : integer;
  signal kab : std_logic_vector(1 to 3);
  signal fneb : integer_vector(0 downto 0);
begin
  cgiib : entity work.bpo
    port map (crcxrapian => fneb, wvop => kab, gadefhkey => lu, dg => nkmtuxojug);
  uns : entity work.bpo
    port map (crcxrapian => yfsu, wvop => hazu, gadefhkey => fectrw, dg => v);
  wudmdn : entity work.bpo
    port map (crcxrapian => yfsu, wvop => utcz, gadefhkey => i, dg => v);
  
  -- Single-driven assignments
  pfudrwvx <= WARNING;
  yfsu <= fneb;
  fneb <= fneb;
  
  -- Multi-driven assignments
  utcz <= ('W', '1', 'U');
  hazu <= hazu;
  v <= ('1', 'H', 'U', 'W', 'U');
  r <= 'L';
end ble;

library ieee;
use ieee.std_logic_1164.all;

entity lfa is
  port (cvttfnm : inout std_logic_vector(1 to 3); g : in time; z : buffer std_logic_vector(1 to 4); pqzmti : buffer boolean);
end lfa;

library ieee;
use ieee.std_logic_1164.all;

architecture pylaer of lfa is
  signal yhpqitobo : severity_level;
  signal fwthzu : std_logic_vector(0 to 4);
  signal s : std_logic_vector(3 downto 1);
  signal zo : std_logic;
  signal rjquazbg : integer;
  signal gts : std_logic_vector(1 to 3);
  signal qykoqipsms : std_logic_vector(4 downto 0);
  signal cac : integer;
  signal jdo : std_logic_vector(1 to 3);
  signal hbnprjz : integer_vector(0 downto 0);
begin
  ozls : entity work.bpo
    port map (crcxrapian => hbnprjz, wvop => jdo, gadefhkey => cac, dg => qykoqipsms);
  tpq : entity work.bpo
    port map (crcxrapian => hbnprjz, wvop => gts, gadefhkey => rjquazbg, dg => qykoqipsms);
  juoyeqkjba : entity work.bklbvd
    port map (r => zo, hazu => s, v => fwthzu, pfudrwvx => yhpqitobo);
  
  -- Multi-driven assignments
  qykoqipsms <= "HX000";
  cvttfnm <= cvttfnm;
end pylaer;



-- Seed after: 4292946466380614919,15795020531041709203
