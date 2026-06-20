-- Seed: 12335250824993826901,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity ji is
  port (ctkkbu : buffer std_logic_vector(4 downto 4); zkurnurocv : inout real; br : out integer_vector(2 downto 4));
end ji;

architecture uppm of ji is
  
begin
  -- Single-driven assignments
  zkurnurocv <= 3_2.2_2;
  br <= (others => 0);
  
  -- Multi-driven assignments
  ctkkbu <= "0";
  ctkkbu <= "Z";
end uppm;

library ieee;
use ieee.std_logic_1164.all;

entity yp is
  port (e : in std_logic_vector(1 downto 2); yrygihskct : out std_logic; oghbqvs : out std_logic);
end yp;

architecture tdldqd of yp is
  
begin
  -- Multi-driven assignments
  oghbqvs <= 'H';
  oghbqvs <= 'U';
end tdldqd;

library ieee;
use ieee.std_logic_1164.all;

entity oxg is
  port (eerkrpty : buffer std_logic_vector(1 to 0); mjta : buffer real);
end oxg;

library ieee;
use ieee.std_logic_1164.all;

architecture qvl of oxg is
  signal myjhnot : integer_vector(2 downto 4);
  signal qbbpgw : std_logic_vector(4 downto 4);
begin
  seo : entity work.ji
    port map (ctkkbu => qbbpgw, zkurnurocv => mjta, br => myjhnot);
  
  -- Multi-driven assignments
  eerkrpty <= (others => '0');
end qvl;

entity qet is
  port (wrtpow : buffer real);
end qet;

library ieee;
use ieee.std_logic_1164.all;

architecture bfiytudj of qet is
  signal dang : std_logic;
  signal uep : std_logic_vector(1 downto 2);
  signal ppxgsoj : integer_vector(2 downto 4);
  signal jpsnr : std_logic_vector(4 downto 4);
begin
  cksicjsqpf : entity work.ji
    port map (ctkkbu => jpsnr, zkurnurocv => wrtpow, br => ppxgsoj);
  kvhwhhjy : entity work.yp
    port map (e => uep, yrygihskct => dang, oghbqvs => dang);
  
  -- Multi-driven assignments
  jpsnr <= (others => '-');
  dang <= 'H';
end bfiytudj;



-- Seed after: 2792438413008203920,17924494779688682807
