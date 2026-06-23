-- Seed: 3294434212473624800,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity zgdmtteur is
  port (jq : out integer_vector(4 downto 4); livhsfvrz : linkage integer; f : out std_logic_vector(1 downto 3));
end zgdmtteur;

architecture vuy of zgdmtteur is
  
begin
  -- Single-driven assignments
  jq <= (others => 8#76#);
  
  -- Multi-driven assignments
  f <= (others => '0');
  f <= (others => '0');
  f <= "";
end vuy;

entity nvdzr is
  port (xqdtqkm : linkage integer);
end nvdzr;

library ieee;
use ieee.std_logic_1164.all;

architecture hvstymgqqt of nvdzr is
  signal pinkalfshy : std_logic_vector(1 downto 3);
  signal qaxbfn : integer;
  signal tznf : integer_vector(4 downto 4);
  signal kcfkyegsv : std_logic_vector(1 downto 3);
  signal tjfbzhgai : integer;
  signal revtwx : integer_vector(4 downto 4);
  signal fcxlzbk : std_logic_vector(1 downto 3);
  signal gkhnmbxpy : integer;
  signal cqxtrvid : integer_vector(4 downto 4);
begin
  uvtrxb : entity work.zgdmtteur
    port map (jq => cqxtrvid, livhsfvrz => gkhnmbxpy, f => fcxlzbk);
  apxmbt : entity work.zgdmtteur
    port map (jq => revtwx, livhsfvrz => tjfbzhgai, f => kcfkyegsv);
  fbclwby : entity work.zgdmtteur
    port map (jq => tznf, livhsfvrz => qaxbfn, f => pinkalfshy);
  
  -- Multi-driven assignments
  pinkalfshy <= (others => '0');
  kcfkyegsv <= (others => '0');
  pinkalfshy <= (others => '0');
  kcfkyegsv <= "";
end hvstymgqqt;



-- Seed after: 12619361187756918633,8421704836678237495
