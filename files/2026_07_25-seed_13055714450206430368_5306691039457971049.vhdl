-- Seed: 13055714450206430368,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (qjuentrxvc : buffer std_logic);
end k;

architecture sizrni of k is
  
begin
  -- Multi-driven assignments
  qjuentrxvc <= 'L';
  qjuentrxvc <= '-';
end sizrni;

library ieee;
use ieee.std_logic_1164.all;

entity abfqrb is
  port ( rryuxjmkxa : out std_logic_vector(1 downto 4)
  ; ukcsohwety : inout std_logic_vector(2 to 3)
  ; zocgodshdz : inout bit_vector(0 downto 2)
  ; cgeezii : inout integer
  );
end abfqrb;

library ieee;
use ieee.std_logic_1164.all;

architecture v of abfqrb is
  signal s : std_logic;
  signal rgtqjvg : std_logic;
begin
  qgchhh : entity work.k
    port map (qjuentrxvc => rgtqjvg);
  snkki : entity work.k
    port map (qjuentrxvc => s);
  
  -- Single-driven assignments
  cgeezii <= 312;
  zocgodshdz <= (others => '0');
  
  -- Multi-driven assignments
  rryuxjmkxa <= rryuxjmkxa;
  ukcsohwety <= "LZ";
end v;

library ieee;
use ieee.std_logic_1164.all;

entity trbs is
  port (luaduxygdw : inout integer; y : linkage std_logic_vector(0 to 4));
end trbs;

library ieee;
use ieee.std_logic_1164.all;

architecture fddhiklj of trbs is
  signal ze : std_logic;
  signal idyznapdf : std_logic;
  signal gztt : integer;
  signal iopklno : bit_vector(0 downto 2);
  signal jlilhrkfb : std_logic_vector(2 to 3);
  signal uiycqvh : std_logic_vector(1 downto 4);
begin
  njzuze : entity work.abfqrb
    port map (rryuxjmkxa => uiycqvh, ukcsohwety => jlilhrkfb, zocgodshdz => iopklno, cgeezii => gztt);
  xlimtq : entity work.k
    port map (qjuentrxvc => idyznapdf);
  klrudqwv : entity work.k
    port map (qjuentrxvc => ze);
  
  -- Single-driven assignments
  luaduxygdw <= luaduxygdw;
  
  -- Multi-driven assignments
  uiycqvh <= (others => '0');
end fddhiklj;



-- Seed after: 10061273582228226735,5306691039457971049
