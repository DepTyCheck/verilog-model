-- Seed: 14905780186037374552,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity rflhaus is
  port (jphuauu : out std_logic; whyjzma : buffer integer; uisxjtfohm : buffer boolean);
end rflhaus;

architecture lqxapgmsuk of rflhaus is
  
begin
  -- Single-driven assignments
  uisxjtfohm <= FALSE;
  whyjzma <= 16#9#;
  
  -- Multi-driven assignments
  jphuauu <= '-';
  jphuauu <= 'X';
  jphuauu <= '-';
end lqxapgmsuk;

entity wrms is
  port (rji : buffer real);
end wrms;

library ieee;
use ieee.std_logic_1164.all;

architecture dx of wrms is
  signal i : boolean;
  signal odbptnbmhp : integer;
  signal frmwnhnwm : std_logic;
begin
  gtpzpoh : entity work.rflhaus
    port map (jphuauu => frmwnhnwm, whyjzma => odbptnbmhp, uisxjtfohm => i);
  
  -- Single-driven assignments
  rji <= 1.10313;
  
  -- Multi-driven assignments
  frmwnhnwm <= 'Z';
  frmwnhnwm <= 'Z';
end dx;

entity naccnbusyd is
  port (atey : in real; gnzke : inout boolean_vector(1 to 3));
end naccnbusyd;

library ieee;
use ieee.std_logic_1164.all;

architecture oiugdika of naccnbusyd is
  signal gdfxnwgs : boolean;
  signal hnm : integer;
  signal cmy : std_logic;
  signal dr : boolean;
  signal pyrlrz : integer;
  signal c : std_logic;
  signal mnygdh : boolean;
  signal h : integer;
  signal kxdauq : std_logic;
begin
  wyp : entity work.rflhaus
    port map (jphuauu => kxdauq, whyjzma => h, uisxjtfohm => mnygdh);
  aeyfii : entity work.rflhaus
    port map (jphuauu => c, whyjzma => pyrlrz, uisxjtfohm => dr);
  vaxkxixk : entity work.rflhaus
    port map (jphuauu => cmy, whyjzma => hnm, uisxjtfohm => gdfxnwgs);
  
  -- Multi-driven assignments
  cmy <= 'Z';
  kxdauq <= 'L';
end oiugdika;

library ieee;
use ieee.std_logic_1164.all;

entity jqsi is
  port (tqc : buffer integer_vector(0 to 2); lsyum : out std_logic_vector(2 to 0); xhxtv : out std_logic_vector(1 downto 3); ryheu : linkage time);
end jqsi;

library ieee;
use ieee.std_logic_1164.all;

architecture kwwevgq of jqsi is
  signal xqlbkmoqlu : boolean;
  signal cgigqetlo : integer;
  signal rf : boolean;
  signal slurj : integer;
  signal ypuzgraiz : std_logic;
  signal jmclsbhvs : boolean_vector(1 to 3);
  signal n : real;
begin
  lhq : entity work.naccnbusyd
    port map (atey => n, gnzke => jmclsbhvs);
  wj : entity work.rflhaus
    port map (jphuauu => ypuzgraiz, whyjzma => slurj, uisxjtfohm => rf);
  ufopzgt : entity work.rflhaus
    port map (jphuauu => ypuzgraiz, whyjzma => cgigqetlo, uisxjtfohm => xqlbkmoqlu);
  
  -- Single-driven assignments
  tqc <= (2_4, 22, 8#63440#);
  
  -- Multi-driven assignments
  xhxtv <= (others => '0');
  ypuzgraiz <= '1';
end kwwevgq;



-- Seed after: 16593357854495659669,4860866131898729603
