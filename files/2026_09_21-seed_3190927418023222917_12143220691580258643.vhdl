-- Seed: 3190927418023222917,12143220691580258643

entity ic is
  port (usiwu : inout time);
end ic;

architecture gsledaq of ic is
  
begin
  -- Single-driven assignments
  usiwu <= 8#674# us;
end gsledaq;

library ieee;
use ieee.std_logic_1164.all;

entity nqiwaugbv is
  port (kvnvc : linkage integer_vector(4 to 0); fmmncg : linkage std_logic_vector(0 downto 2));
end nqiwaugbv;

architecture ocftwqj of nqiwaugbv is
  signal dtxzkd : time;
  signal wvxke : time;
  signal pfibu : time;
  signal xv : time;
begin
  t : entity work.ic
    port map (usiwu => xv);
  egbvqr : entity work.ic
    port map (usiwu => pfibu);
  zorpaxwi : entity work.ic
    port map (usiwu => wvxke);
  nh : entity work.ic
    port map (usiwu => dtxzkd);
end ocftwqj;

entity nccsmmt is
  port (sheamdexj : in integer; cy : linkage real; te : in integer);
end nccsmmt;

library ieee;
use ieee.std_logic_1164.all;

architecture welfk of nccsmmt is
  signal h : time;
  signal ptq : std_logic_vector(0 downto 2);
  signal pn : integer_vector(4 to 0);
  signal qflxvmgkfh : std_logic_vector(0 downto 2);
  signal ucwf : integer_vector(4 to 0);
  signal wdvzzlecnh : time;
begin
  urfwlp : entity work.ic
    port map (usiwu => wdvzzlecnh);
  vsjro : entity work.nqiwaugbv
    port map (kvnvc => ucwf, fmmncg => qflxvmgkfh);
  bmt : entity work.nqiwaugbv
    port map (kvnvc => pn, fmmncg => ptq);
  dykzvz : entity work.ic
    port map (usiwu => h);
  
  -- Multi-driven assignments
  qflxvmgkfh <= (others => '0');
  qflxvmgkfh <= qflxvmgkfh;
  qflxvmgkfh <= "";
end welfk;



-- Seed after: 6666125096254624735,12143220691580258643
