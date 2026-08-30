-- Seed: 3330594926900744190,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity suhysnshud is
  port (atnd : inout std_logic; wzfzww : buffer boolean_vector(0 downto 0); xzokkmpdo : out time; qgfxissyoe : buffer time);
end suhysnshud;

architecture nfgkpsmn of suhysnshud is
  
begin
  -- Single-driven assignments
  xzokkmpdo <= qgfxissyoe;
  wzfzww <= (others => TRUE);
  qgfxissyoe <= qgfxissyoe;
  
  -- Multi-driven assignments
  atnd <= 'U';
  atnd <= 'U';
  atnd <= 'U';
  atnd <= '-';
end nfgkpsmn;

entity ooeclweyih is
  port (qp : buffer real; ftwky : inout real);
end ooeclweyih;

library ieee;
use ieee.std_logic_1164.all;

architecture bwondwokt of ooeclweyih is
  signal tccjcxmuf : time;
  signal sdq : time;
  signal fol : boolean_vector(0 downto 0);
  signal nt : std_logic;
  signal dkjc : time;
  signal ubhu : time;
  signal vbfkhodpf : boolean_vector(0 downto 0);
  signal gknendgv : std_logic;
  signal ggrqks : time;
  signal vwll : time;
  signal r : boolean_vector(0 downto 0);
  signal vh : std_logic;
begin
  egsi : entity work.suhysnshud
    port map (atnd => vh, wzfzww => r, xzokkmpdo => vwll, qgfxissyoe => ggrqks);
  ykfuh : entity work.suhysnshud
    port map (atnd => gknendgv, wzfzww => vbfkhodpf, xzokkmpdo => ubhu, qgfxissyoe => dkjc);
  koidafofee : entity work.suhysnshud
    port map (atnd => nt, wzfzww => fol, xzokkmpdo => sdq, qgfxissyoe => tccjcxmuf);
  
  -- Multi-driven assignments
  gknendgv <= vh;
  vh <= '1';
end bwondwokt;



-- Seed after: 3553684734893044008,4080032123900078489
