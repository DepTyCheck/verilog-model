-- Seed: 8150357022063497965,14108379317686958507

entity qf is
  port (jdk : inout real_vector(3 to 3); my : in integer; ec : out real);
end qf;

architecture nhciozyhv of qf is
  
begin
  -- Single-driven assignments
  ec <= ec;
  jdk <= jdk;
end nhciozyhv;

entity dfynfhqm is
  port (kitvod : linkage integer);
end dfynfhqm;

architecture ercbp of dfynfhqm is
  signal muvb : real;
  signal dxe : integer;
  signal fvcexlmu : real_vector(3 to 3);
  signal zrukzadn : real;
  signal tnlglas : real_vector(3 to 3);
  signal wlokwt : real;
  signal cxibhwnli : integer;
  signal cjecpcyoa : real_vector(3 to 3);
begin
  xrtffgdofr : entity work.qf
    port map (jdk => cjecpcyoa, my => cxibhwnli, ec => wlokwt);
  vrrsfge : entity work.qf
    port map (jdk => tnlglas, my => cxibhwnli, ec => zrukzadn);
  qh : entity work.qf
    port map (jdk => fvcexlmu, my => dxe, ec => muvb);
end ercbp;

library ieee;
use ieee.std_logic_1164.all;

entity etgsq is
  port (yjfuqkn : out integer; kqe : buffer std_logic; ydsz : inout time; bjbgrlsz : out std_logic_vector(0 to 0));
end etgsq;

architecture nyrm of etgsq is
  signal lqruvygaiq : real;
  signal iae : integer;
  signal ifltoymdv : real_vector(3 to 3);
  signal ho : real;
  signal urxmqdgc : integer;
  signal fzmmwypwc : real_vector(3 to 3);
  signal m : integer;
begin
  odmsoul : entity work.dfynfhqm
    port map (kitvod => m);
  x : entity work.qf
    port map (jdk => fzmmwypwc, my => urxmqdgc, ec => ho);
  uqgje : entity work.qf
    port map (jdk => ifltoymdv, my => iae, ec => lqruvygaiq);
  
  -- Single-driven assignments
  ydsz <= ydsz;
  
  -- Multi-driven assignments
  bjbgrlsz <= bjbgrlsz;
  kqe <= kqe;
end nyrm;



-- Seed after: 10484081202378518488,14108379317686958507
