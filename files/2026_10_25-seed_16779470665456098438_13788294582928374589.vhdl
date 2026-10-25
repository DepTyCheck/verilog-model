-- Seed: 16779470665456098438,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity hjzytyik is
  port ( yf : buffer std_logic_vector(1 downto 0)
  ; uoeqxtxb : linkage integer
  ; lhgiuzdb : linkage std_logic_vector(4 downto 1)
  ; xukmduf : linkage integer
  );
end hjzytyik;

architecture nmyjzh of hjzytyik is
  
begin
  -- Multi-driven assignments
  yf <= "U1";
  yf <= "XH";
  yf <= ('Z', 'L');
  yf <= yf;
end nmyjzh;

library ieee;
use ieee.std_logic_1164.all;

entity ddoytjvvl is
  port (bh : inout std_logic; zcmmakdq : buffer std_logic; hrjm : buffer integer; ogn : in std_logic_vector(4 to 2));
end ddoytjvvl;

library ieee;
use ieee.std_logic_1164.all;

architecture xjdzzayfag of ddoytjvvl is
  signal irflpfued : integer;
  signal xosfs : integer;
  signal tzriiqak : std_logic_vector(1 downto 0);
  signal xihrj : std_logic_vector(4 downto 1);
  signal jxwbbh : integer;
  signal clcnnswra : std_logic_vector(1 downto 0);
begin
  pkr : entity work.hjzytyik
    port map (yf => clcnnswra, uoeqxtxb => jxwbbh, lhgiuzdb => xihrj, xukmduf => hrjm);
  ukfisbk : entity work.hjzytyik
    port map (yf => tzriiqak, uoeqxtxb => xosfs, lhgiuzdb => xihrj, xukmduf => irflpfued);
  
  -- Multi-driven assignments
  zcmmakdq <= '0';
  bh <= zcmmakdq;
  tzriiqak <= "-X";
  zcmmakdq <= 'H';
end xjdzzayfag;

library ieee;
use ieee.std_logic_1164.all;

entity lsiojslfjn is
  port (cfqiflo : linkage std_logic_vector(2 downto 0));
end lsiojslfjn;

library ieee;
use ieee.std_logic_1164.all;

architecture tcjbgr of lsiojslfjn is
  signal xilurvkbig : std_logic_vector(4 to 2);
  signal yeenl : integer;
  signal ke : std_logic;
  signal kminm : std_logic;
  signal j : integer;
  signal i : std_logic_vector(4 downto 1);
  signal e : integer;
  signal x : std_logic_vector(1 downto 0);
  signal zzyvhuogx : integer;
  signal ipanofxktj : std_logic_vector(4 downto 1);
  signal eevyqguf : integer;
  signal namm : std_logic_vector(1 downto 0);
begin
  qpzxxkzoma : entity work.hjzytyik
    port map (yf => namm, uoeqxtxb => eevyqguf, lhgiuzdb => ipanofxktj, xukmduf => zzyvhuogx);
  tciovdq : entity work.hjzytyik
    port map (yf => x, uoeqxtxb => e, lhgiuzdb => i, xukmduf => j);
  adnag : entity work.ddoytjvvl
    port map (bh => kminm, zcmmakdq => ke, hrjm => yeenl, ogn => xilurvkbig);
  
  -- Multi-driven assignments
  xilurvkbig <= (others => '0');
  ipanofxktj <= "XXXX";
  namm <= "UH";
  ke <= 'X';
end tcjbgr;



-- Seed after: 8818662712704534651,13788294582928374589
