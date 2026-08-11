-- Seed: 8994519803911290338,10594830431004325987

entity g is
  port (lrfxlsxud : inout real_vector(2 to 3));
end g;

architecture vfuo of g is
  
begin
  
end vfuo;

library ieee;
use ieee.std_logic_1164.all;

entity lebc is
  port (esfc : buffer std_logic_vector(4 to 3));
end lebc;

architecture yebgxiv of lebc is
  signal mgcucpjn : real_vector(2 to 3);
  signal myrghlo : real_vector(2 to 3);
begin
  ipvbivqp : entity work.g
    port map (lrfxlsxud => myrghlo);
  xopksrv : entity work.g
    port map (lrfxlsxud => mgcucpjn);
  
  -- Multi-driven assignments
  esfc <= (others => '0');
  esfc <= esfc;
end yebgxiv;

library ieee;
use ieee.std_logic_1164.all;

entity td is
  port (ipw : in std_logic; lgvnrlvpb : out real; e : out boolean);
end td;

library ieee;
use ieee.std_logic_1164.all;

architecture ojezgy of td is
  signal rnfefwdv : std_logic_vector(4 to 3);
  signal ojwhil : real_vector(2 to 3);
  signal xlkxcyxqpe : real_vector(2 to 3);
begin
  o : entity work.g
    port map (lrfxlsxud => xlkxcyxqpe);
  fjslddm : entity work.g
    port map (lrfxlsxud => ojwhil);
  zndhhgkbud : entity work.lebc
    port map (esfc => rnfefwdv);
end ojezgy;

library ieee;
use ieee.std_logic_1164.all;

entity icarq is
  port (ra : out time; nwzbqvmfye : linkage std_logic; dhbz : inout time; de : out integer);
end icarq;

architecture kxzyhavtjp of icarq is
  signal hivnyje : real_vector(2 to 3);
  signal dkat : real_vector(2 to 3);
begin
  gi : entity work.g
    port map (lrfxlsxud => dkat);
  qskdtwxua : entity work.g
    port map (lrfxlsxud => hivnyje);
  
  -- Single-driven assignments
  de <= 002;
  dhbz <= 8#7.2377# ms;
  ra <= 8#65215.04347# us;
end kxzyhavtjp;



-- Seed after: 941064277144568380,10594830431004325987
