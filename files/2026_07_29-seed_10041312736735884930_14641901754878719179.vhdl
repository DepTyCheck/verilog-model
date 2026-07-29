-- Seed: 10041312736735884930,14641901754878719179

entity vzaqsi is
  port (ed : linkage real; ujt : inout integer);
end vzaqsi;

architecture hnxepslk of vzaqsi is
  
begin
  
end hnxepslk;

library ieee;
use ieee.std_logic_1164.all;

entity cl is
  port (eapa : out std_logic_vector(2 downto 2); mzjfhmz : out time);
end cl;

architecture dk of cl is
  signal qridcvcgt : integer;
  signal dwvynpupmy : real;
begin
  lriggukokc : entity work.vzaqsi
    port map (ed => dwvynpupmy, ujt => qridcvcgt);
end dk;

entity ph is
  port (ltrk : buffer real; peqz : inout time_vector(2 to 1));
end ph;

architecture gbt of ph is
  signal ktoyfu : integer;
  signal bugkbvwd : integer;
  signal vbgey : real;
  signal jkfxa : integer;
  signal lptlt : real;
  signal ti : integer;
  signal lrfuozgedk : real;
begin
  z : entity work.vzaqsi
    port map (ed => lrfuozgedk, ujt => ti);
  yxodywdls : entity work.vzaqsi
    port map (ed => lptlt, ujt => jkfxa);
  pjcb : entity work.vzaqsi
    port map (ed => vbgey, ujt => bugkbvwd);
  agd : entity work.vzaqsi
    port map (ed => ltrk, ujt => ktoyfu);
  
  -- Single-driven assignments
  peqz <= (others => 0 ns);
end gbt;

entity b is
  port (mhyfvzucp : out real; pvr : linkage boolean; fq : inout time_vector(3 to 1));
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture agzprasjo of b is
  signal cuczx : integer;
  signal yjmzee : real;
  signal as : integer;
  signal ckkexpa : time;
  signal prnqp : std_logic_vector(2 downto 2);
begin
  uwojrlpl : entity work.cl
    port map (eapa => prnqp, mzjfhmz => ckkexpa);
  nqedclyic : entity work.vzaqsi
    port map (ed => mhyfvzucp, ujt => as);
  egq : entity work.vzaqsi
    port map (ed => yjmzee, ujt => cuczx);
  
  -- Single-driven assignments
  fq <= (others => 0 ns);
  
  -- Multi-driven assignments
  prnqp <= prnqp;
  prnqp <= "0";
end agzprasjo;



-- Seed after: 12376106612394869537,14641901754878719179
