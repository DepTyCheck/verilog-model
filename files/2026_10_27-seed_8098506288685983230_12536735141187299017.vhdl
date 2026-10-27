-- Seed: 8098506288685983230,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity eynkyrzzyy is
  port (zcorvfbybr : buffer std_logic_vector(0 downto 1));
end eynkyrzzyy;

architecture yfhkuzfno of eynkyrzzyy is
  
begin
  -- Multi-driven assignments
  zcorvfbybr <= zcorvfbybr;
  zcorvfbybr <= "";
  zcorvfbybr <= (others => '0');
end yfhkuzfno;

library ieee;
use ieee.std_logic_1164.all;

entity tmfplro is
  port (hsildllr : in std_logic_vector(4 to 0); tt : inout integer_vector(4 to 3); soyalykuza : in bit_vector(2 downto 4));
end tmfplro;

library ieee;
use ieee.std_logic_1164.all;

architecture rtcbqvnjc of tmfplro is
  signal ygxvhjxg : std_logic_vector(0 downto 1);
begin
  xfvg : entity work.eynkyrzzyy
    port map (zcorvfbybr => ygxvhjxg);
  
  -- Single-driven assignments
  tt <= tt;
  
  -- Multi-driven assignments
  ygxvhjxg <= (others => '0');
end rtcbqvnjc;

entity ebodocvba is
  port (xjiykkelpu : out time_vector(3 to 4));
end ebodocvba;

library ieee;
use ieee.std_logic_1164.all;

architecture qle of ebodocvba is
  signal rmnidwft : integer_vector(4 to 3);
  signal xlfbfpaxcq : bit_vector(2 downto 4);
  signal gd : integer_vector(4 to 3);
  signal dopnapb : std_logic_vector(4 to 0);
begin
  ai : entity work.tmfplro
    port map (hsildllr => dopnapb, tt => gd, soyalykuza => xlfbfpaxcq);
  clrhyrbwen : entity work.tmfplro
    port map (hsildllr => dopnapb, tt => rmnidwft, soyalykuza => xlfbfpaxcq);
  
  -- Single-driven assignments
  xjiykkelpu <= (3_1_2_2_3.01 ns, 1311 ns);
  xlfbfpaxcq <= (others => '0');
  
  -- Multi-driven assignments
  dopnapb <= "";
end qle;

library ieee;
use ieee.std_logic_1164.all;

entity ecbao is
  port (xdessigmur : out std_logic; yayzgheaym : buffer time);
end ecbao;

library ieee;
use ieee.std_logic_1164.all;

architecture kjpei of ecbao is
  signal lcxyfhbqit : bit_vector(2 downto 4);
  signal ptecqsys : integer_vector(4 to 3);
  signal h : std_logic_vector(4 to 0);
  signal nxeaknoc : std_logic_vector(0 downto 1);
begin
  nwbvwputp : entity work.eynkyrzzyy
    port map (zcorvfbybr => nxeaknoc);
  k : entity work.tmfplro
    port map (hsildllr => h, tt => ptecqsys, soyalykuza => lcxyfhbqit);
  svrwq : entity work.eynkyrzzyy
    port map (zcorvfbybr => nxeaknoc);
  
  -- Multi-driven assignments
  h <= (others => '0');
end kjpei;



-- Seed after: 5106518437722363734,12536735141187299017
