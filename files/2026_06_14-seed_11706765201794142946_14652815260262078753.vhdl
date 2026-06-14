-- Seed: 11706765201794142946,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity ksllmwma is
  port (xceguxi : linkage std_logic_vector(4 to 0));
end ksllmwma;

architecture n of ksllmwma is
  
begin
  
end n;

library ieee;
use ieee.std_logic_1164.all;

entity mdcjyyhj is
  port (ooibvhmvh : linkage std_logic_vector(1 downto 0));
end mdcjyyhj;

library ieee;
use ieee.std_logic_1164.all;

architecture rlepocpgna of mdcjyyhj is
  signal whd : std_logic_vector(4 to 0);
begin
  mqkrxgwnp : entity work.ksllmwma
    port map (xceguxi => whd);
  qa : entity work.ksllmwma
    port map (xceguxi => whd);
end rlepocpgna;

entity rq is
  port (inikkiv : buffer time_vector(1 to 1); nrvyqxcm : buffer time);
end rq;

library ieee;
use ieee.std_logic_1164.all;

architecture funelrkr of rq is
  signal rbecnnpf : std_logic_vector(4 to 0);
  signal opwkmfler : std_logic_vector(1 downto 0);
  signal tzklqa : std_logic_vector(4 to 0);
begin
  mujptsasge : entity work.ksllmwma
    port map (xceguxi => tzklqa);
  hsw : entity work.mdcjyyhj
    port map (ooibvhmvh => opwkmfler);
  lzv : entity work.ksllmwma
    port map (xceguxi => rbecnnpf);
  
  -- Single-driven assignments
  nrvyqxcm <= 0_3_3 ms;
  inikkiv <= (others => 16#F_C.86C89# ns);
  
  -- Multi-driven assignments
  rbecnnpf <= (others => '0');
  tzklqa <= (others => '0');
end funelrkr;

entity o is
  port (bvnf : inout real; tli : buffer time; quv : inout bit_vector(1 downto 1));
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture sorgmr of o is
  signal tvcfbezmp : std_logic_vector(4 to 0);
  signal yszipeg : std_logic_vector(4 to 0);
begin
  kilxepqtgh : entity work.ksllmwma
    port map (xceguxi => yszipeg);
  yp : entity work.ksllmwma
    port map (xceguxi => tvcfbezmp);
  
  -- Single-driven assignments
  quv <= (others => '1');
  
  -- Multi-driven assignments
  yszipeg <= "";
end sorgmr;



-- Seed after: 10312194280071417787,14652815260262078753
