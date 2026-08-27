-- Seed: 1743441499351143027,6299883410057943775

entity koxsjtlmm is
  port (mmcpeoj : in string(4 to 5); gqzts : linkage bit_vector(4 to 0); ojersbcen : linkage real_vector(1 to 2));
end koxsjtlmm;

architecture ccvcydj of koxsjtlmm is
  
begin
  
end ccvcydj;

entity ktv is
  port (lku : out real);
end ktv;

architecture swqmqou of ktv is
  signal ecneptsxm : real_vector(1 to 2);
  signal dvhthetnv : bit_vector(4 to 0);
  signal xhqbsz : string(4 to 5);
  signal jakk : real_vector(1 to 2);
  signal duixjaba : bit_vector(4 to 0);
  signal jmbthv : real_vector(1 to 2);
  signal slyrsgba : bit_vector(4 to 0);
  signal sazw : string(4 to 5);
begin
  yvtcdszon : entity work.koxsjtlmm
    port map (mmcpeoj => sazw, gqzts => slyrsgba, ojersbcen => jmbthv);
  fay : entity work.koxsjtlmm
    port map (mmcpeoj => sazw, gqzts => duixjaba, ojersbcen => jakk);
  pauetgrsxl : entity work.koxsjtlmm
    port map (mmcpeoj => xhqbsz, gqzts => dvhthetnv, ojersbcen => ecneptsxm);
  
  -- Single-driven assignments
  lku <= lku;
  xhqbsz <= ('d', 'l');
  sazw <= "rr";
end swqmqou;

library ieee;
use ieee.std_logic_1164.all;

entity rpef is
  port (jigpr : out std_logic_vector(3 downto 3); so : out std_logic; w : in integer);
end rpef;

architecture huxue of rpef is
  signal kgiwykjzqq : real_vector(1 to 2);
  signal zebfltdvnc : bit_vector(4 to 0);
  signal hqmv : string(4 to 5);
begin
  dapwa : entity work.koxsjtlmm
    port map (mmcpeoj => hqmv, gqzts => zebfltdvnc, ojersbcen => kgiwykjzqq);
end huxue;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (riktopcrdb : in std_logic);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture jztyvs of x is
  signal olf : real_vector(1 to 2);
  signal vxjwe : bit_vector(4 to 0);
  signal sac : string(4 to 5);
  signal nz : integer;
  signal so : std_logic;
  signal yvfwmrybh : std_logic_vector(3 downto 3);
begin
  coygfyjoyd : entity work.rpef
    port map (jigpr => yvfwmrybh, so => so, w => nz);
  jgvm : entity work.koxsjtlmm
    port map (mmcpeoj => sac, gqzts => vxjwe, ojersbcen => olf);
  
  -- Single-driven assignments
  nz <= nz;
  sac <= sac;
  
  -- Multi-driven assignments
  yvfwmrybh <= "0";
  yvfwmrybh <= yvfwmrybh;
end jztyvs;



-- Seed after: 6158339994705419102,6299883410057943775
