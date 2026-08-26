-- Seed: 3772783124861813172,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity agicrok is
  port (krw : buffer std_logic);
end agicrok;

architecture fphd of agicrok is
  
begin
  -- Multi-driven assignments
  krw <= 'L';
  krw <= krw;
  krw <= 'Z';
end fphd;

library ieee;
use ieee.std_logic_1164.all;

entity bdeegqdy is
  port (e : in std_logic_vector(4 downto 0); zxvnf : in bit);
end bdeegqdy;

library ieee;
use ieee.std_logic_1164.all;

architecture gtgkkehzg of bdeegqdy is
  signal zel : std_logic;
begin
  oepaqhhdo : entity work.agicrok
    port map (krw => zel);
  iqvpprc : entity work.agicrok
    port map (krw => zel);
end gtgkkehzg;

library ieee;
use ieee.std_logic_1164.all;

entity lreqb is
  port (rm : inout std_logic_vector(4 to 4));
end lreqb;

library ieee;
use ieee.std_logic_1164.all;

architecture pdaoqpc of lreqb is
  signal iuky : bit;
  signal ajjechzqb : std_logic_vector(4 downto 0);
  signal aowach : std_logic;
  signal q : std_logic;
begin
  zgokps : entity work.agicrok
    port map (krw => q);
  oj : entity work.agicrok
    port map (krw => aowach);
  onlajpfumn : entity work.bdeegqdy
    port map (e => ajjechzqb, zxvnf => iuky);
  kwogedaiiw : entity work.bdeegqdy
    port map (e => ajjechzqb, zxvnf => iuky);
  
  -- Single-driven assignments
  iuky <= '1';
  
  -- Multi-driven assignments
  q <= q;
  ajjechzqb <= "XW-1Z";
  rm <= rm;
end pdaoqpc;



-- Seed after: 8807827860655487130,6000118208082478503
