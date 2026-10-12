-- Seed: 5081078384170656242,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (pc : inout std_logic_vector(2 downto 3); frcgnmgc : buffer std_logic_vector(1 downto 3); wsyus : in std_logic);
end o;

architecture sqgmvoq of o is
  
begin
  -- Multi-driven assignments
  frcgnmgc <= "";
  frcgnmgc <= frcgnmgc;
  pc <= frcgnmgc;
end sqgmvoq;

entity mxpgiqna is
  port (hgbxyfitcf : inout boolean; iyrwzs : buffer time);
end mxpgiqna;

library ieee;
use ieee.std_logic_1164.all;

architecture cxcjsurlzi of mxpgiqna is
  signal utwlzekqte : std_logic_vector(1 downto 3);
  signal hxkxaay : std_logic;
  signal ies : std_logic_vector(2 downto 3);
begin
  xccdnv : entity work.o
    port map (pc => ies, frcgnmgc => ies, wsyus => hxkxaay);
  xngpk : entity work.o
    port map (pc => ies, frcgnmgc => utwlzekqte, wsyus => hxkxaay);
  
  -- Single-driven assignments
  iyrwzs <= iyrwzs;
  hgbxyfitcf <= hgbxyfitcf;
  
  -- Multi-driven assignments
  ies <= (others => '0');
  ies <= "";
  ies <= ies;
end cxcjsurlzi;

entity so is
  port (b : inout real; ulrx : inout severity_level);
end so;

library ieee;
use ieee.std_logic_1164.all;

architecture anmkoyzmx of so is
  signal tdynnridby : std_logic_vector(1 downto 3);
  signal gmseg : std_logic;
  signal w : std_logic_vector(2 downto 3);
  signal volbn : std_logic_vector(1 downto 3);
begin
  qgkgvqoxe : entity work.o
    port map (pc => volbn, frcgnmgc => w, wsyus => gmseg);
  sb : entity work.o
    port map (pc => volbn, frcgnmgc => volbn, wsyus => gmseg);
  utmuv : entity work.o
    port map (pc => w, frcgnmgc => tdynnridby, wsyus => gmseg);
  
  -- Multi-driven assignments
  gmseg <= gmseg;
end anmkoyzmx;

entity qlenpm is
  port (uxpi : in integer; pkppxehf : in integer);
end qlenpm;

library ieee;
use ieee.std_logic_1164.all;

architecture bclac of qlenpm is
  signal z : severity_level;
  signal nfmaoozlwd : real;
  signal ukqc : std_logic;
  signal uzstacse : std_logic_vector(1 downto 3);
  signal yboni : time;
  signal tnz : boolean;
begin
  mvpsqfp : entity work.mxpgiqna
    port map (hgbxyfitcf => tnz, iyrwzs => yboni);
  ddlrxgx : entity work.o
    port map (pc => uzstacse, frcgnmgc => uzstacse, wsyus => ukqc);
  svbfslxfk : entity work.so
    port map (b => nfmaoozlwd, ulrx => z);
  
  -- Multi-driven assignments
  uzstacse <= uzstacse;
  uzstacse <= "";
  uzstacse <= "";
end bclac;



-- Seed after: 15837092133744601695,3426943171337919827
