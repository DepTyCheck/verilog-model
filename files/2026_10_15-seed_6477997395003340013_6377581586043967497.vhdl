-- Seed: 6477997395003340013,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity rlsvvdo is
  port (y : inout real; symlc : out character; qsdn : linkage std_logic_vector(4 downto 0); ybld : buffer string(5 to 3));
end rlsvvdo;

architecture jyjfdlvj of rlsvvdo is
  
begin
  -- Single-driven assignments
  ybld <= (others => ' ');
  y <= y;
  symlc <= 'j';
end jyjfdlvj;

library ieee;
use ieee.std_logic_1164.all;

entity gvgrssx is
  port (j : out character; gioyuxjui : in bit; eznfa : inout std_logic_vector(1 downto 1); hpsoyrpr : buffer std_logic_vector(4 to 0));
end gvgrssx;

library ieee;
use ieee.std_logic_1164.all;

architecture tlbrmrw of gvgrssx is
  signal b : string(5 to 3);
  signal xcbctlw : character;
  signal mgfdxbmd : real;
  signal rcrcoqpw : string(5 to 3);
  signal oucsxp : real;
  signal yzmrkbvple : string(5 to 3);
  signal yy : std_logic_vector(4 downto 0);
  signal gb : character;
  signal lvcslq : real;
  signal dkrdfyhbmd : string(5 to 3);
  signal vgqtdgsk : std_logic_vector(4 downto 0);
  signal wfwcrdr : character;
  signal at : real;
begin
  qymle : entity work.rlsvvdo
    port map (y => at, symlc => wfwcrdr, qsdn => vgqtdgsk, ybld => dkrdfyhbmd);
  jy : entity work.rlsvvdo
    port map (y => lvcslq, symlc => gb, qsdn => yy, ybld => yzmrkbvple);
  xal : entity work.rlsvvdo
    port map (y => oucsxp, symlc => j, qsdn => vgqtdgsk, ybld => rcrcoqpw);
  tylfr : entity work.rlsvvdo
    port map (y => mgfdxbmd, symlc => xcbctlw, qsdn => yy, ybld => b);
  
  -- Multi-driven assignments
  hpsoyrpr <= (others => '0');
  hpsoyrpr <= (others => '0');
  vgqtdgsk <= vgqtdgsk;
  vgqtdgsk <= ('U', '-', 'W', 'U', 'X');
end tlbrmrw;



-- Seed after: 7131714362510128552,6377581586043967497
