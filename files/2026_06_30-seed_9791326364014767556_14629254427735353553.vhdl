-- Seed: 9791326364014767556,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity pnig is
  port (vsxop : linkage time_vector(3 to 1); tzya : out std_logic_vector(1 downto 3));
end pnig;

architecture ow of pnig is
  
begin
  -- Multi-driven assignments
  tzya <= (others => '0');
  tzya <= (others => '0');
  tzya <= (others => '0');
end ow;

library ieee;
use ieee.std_logic_1164.all;

entity gfduj is
  port (weichk : in std_logic_vector(2 to 0); cxahb : buffer time; nozp : linkage time; nfhvo : inout real);
end gfduj;

library ieee;
use ieee.std_logic_1164.all;

architecture xyqnz of gfduj is
  signal osvwv : std_logic_vector(1 downto 3);
  signal bcx : time_vector(3 to 1);
  signal ysutais : time_vector(3 to 1);
  signal s : std_logic_vector(1 downto 3);
  signal tynnsurll : time_vector(3 to 1);
begin
  jyzxjud : entity work.pnig
    port map (vsxop => tynnsurll, tzya => s);
  ksxwb : entity work.pnig
    port map (vsxop => ysutais, tzya => s);
  fgnogc : entity work.pnig
    port map (vsxop => bcx, tzya => osvwv);
  
  -- Single-driven assignments
  nfhvo <= 3100.1;
  cxahb <= 3312 fs;
  
  -- Multi-driven assignments
  osvwv <= "";
end xyqnz;



-- Seed after: 16077284346076711347,14629254427735353553
