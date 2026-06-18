-- Seed: 7452366899836622128,8118127366649987907

entity jgu is
  port (x : in time; znfrjy : out real; qxii : linkage time; kfmu : in time);
end jgu;

architecture dg of jgu is
  
begin
  -- Single-driven assignments
  znfrjy <= 442.434;
end dg;

library ieee;
use ieee.std_logic_1164.all;

entity pphkab is
  port (ual : buffer std_logic; qvhrd : inout real; jzy : buffer boolean);
end pphkab;

architecture pzcwxwixs of pphkab is
  
begin
  -- Single-driven assignments
  jzy <= TRUE;
  qvhrd <= 0_4.3;
  
  -- Multi-driven assignments
  ual <= 'U';
  ual <= 'X';
  ual <= 'U';
  ual <= 'X';
end pzcwxwixs;

entity alomuztb is
  port (vgjx : in integer);
end alomuztb;

library ieee;
use ieee.std_logic_1164.all;

architecture vtkz of alomuztb is
  signal job : boolean;
  signal eqccbvbeq : real;
  signal unv : std_logic;
  signal vptxkvygzw : time;
  signal zfts : real;
  signal skdqzkk : time;
  signal wazaneb : real;
  signal vnd : time;
begin
  eecr : entity work.jgu
    port map (x => vnd, znfrjy => wazaneb, qxii => skdqzkk, kfmu => vnd);
  wrecm : entity work.jgu
    port map (x => vnd, znfrjy => zfts, qxii => vptxkvygzw, kfmu => skdqzkk);
  kcsg : entity work.pphkab
    port map (ual => unv, qvhrd => eqccbvbeq, jzy => job);
  
  -- Single-driven assignments
  vnd <= 4 sec;
  
  -- Multi-driven assignments
  unv <= '0';
  unv <= 'X';
  unv <= '0';
  unv <= 'X';
end vtkz;

library ieee;
use ieee.std_logic_1164.all;

entity smoulzj is
  port (vtz : inout std_logic_vector(2 to 1); yqzmlcqgyd : linkage std_logic);
end smoulzj;

architecture wvqzmqsxrr of smoulzj is
  signal u : time;
  signal hbi : real;
  signal shjq : time;
begin
  usfdprh : entity work.jgu
    port map (x => shjq, znfrjy => hbi, qxii => u, kfmu => shjq);
  
  -- Single-driven assignments
  shjq <= 20 ns;
  
  -- Multi-driven assignments
  vtz <= (others => '0');
  vtz <= (others => '0');
end wvqzmqsxrr;



-- Seed after: 12932540663428140531,8118127366649987907
