-- Seed: 1754075428201468038,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (fcjyvqk : in std_logic; yjpozhz : buffer std_logic_vector(4 to 0));
end a;

architecture pelcwmos of a is
  
begin
  -- Multi-driven assignments
  yjpozhz <= (others => '0');
  yjpozhz <= (others => '0');
  yjpozhz <= "";
  yjpozhz <= (others => '0');
end pelcwmos;

entity qm is
  port (o : inout boolean; hl : inout real; zrkde : inout integer);
end qm;

architecture s of qm is
  
begin
  -- Single-driven assignments
  zrkde <= 4_2;
  o <= TRUE;
  hl <= 2#1_1_1_1.0_1#;
end s;

library ieee;
use ieee.std_logic_1164.all;

entity qsv is
  port (hjaaxlaic : linkage std_logic; nzoyc : inout std_logic; seppu : linkage std_logic_vector(3 to 2));
end qsv;

architecture rnqnqhnop of qsv is
  signal nhpsckgy : integer;
  signal xekysltbz : real;
  signal yknyksmd : boolean;
begin
  byn : entity work.qm
    port map (o => yknyksmd, hl => xekysltbz, zrkde => nhpsckgy);
  
  -- Multi-driven assignments
  nzoyc <= 'W';
  nzoyc <= '-';
  nzoyc <= '-';
  nzoyc <= 'U';
end rnqnqhnop;

library ieee;
use ieee.std_logic_1164.all;

entity eiesjzwnc is
  port ( sksv : out integer_vector(1 downto 1)
  ; vhbkeee : in std_logic_vector(4 to 2)
  ; ubwctfwax : inout bit_vector(4 downto 0)
  ; iygzig : inout std_logic_vector(1 downto 1)
  );
end eiesjzwnc;

library ieee;
use ieee.std_logic_1164.all;

architecture cjpuilfa of eiesjzwnc is
  signal f : std_logic;
  signal ffezafgs : std_logic_vector(4 to 0);
  signal rinnd : std_logic;
  signal ujwavzkkse : std_logic_vector(4 to 0);
  signal wmgzxifzqj : std_logic;
  signal hysbep : std_logic;
begin
  kfocivv : entity work.qsv
    port map (hjaaxlaic => hysbep, nzoyc => wmgzxifzqj, seppu => ujwavzkkse);
  crisa : entity work.a
    port map (fcjyvqk => rinnd, yjpozhz => ffezafgs);
  rtqrpaoezh : entity work.a
    port map (fcjyvqk => f, yjpozhz => ujwavzkkse);
  
  -- Single-driven assignments
  ubwctfwax <= ('1', '0', '0', '0', '1');
  sksv <= (others => 1);
  
  -- Multi-driven assignments
  iygzig <= (others => '1');
end cjpuilfa;



-- Seed after: 13002027883012833103,14652815260262078753
