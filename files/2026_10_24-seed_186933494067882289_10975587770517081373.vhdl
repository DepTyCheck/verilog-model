-- Seed: 186933494067882289,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity nwzp is
  port (mfnlk : buffer std_logic);
end nwzp;

architecture urj of nwzp is
  
begin
  -- Multi-driven assignments
  mfnlk <= 'X';
  mfnlk <= '1';
  mfnlk <= mfnlk;
end urj;

entity kupypmwgli is
  port (g : linkage real; k : inout time; dummf : linkage boolean_vector(3 downto 3); uzwa : out time);
end kupypmwgli;

library ieee;
use ieee.std_logic_1164.all;

architecture ld of kupypmwgli is
  signal o : std_logic;
  signal toxidiz : std_logic;
  signal p : std_logic;
  signal ehdmtuobb : std_logic;
begin
  lawglerfvj : entity work.nwzp
    port map (mfnlk => ehdmtuobb);
  hbqbr : entity work.nwzp
    port map (mfnlk => p);
  biwlnqz : entity work.nwzp
    port map (mfnlk => toxidiz);
  quhwvopyh : entity work.nwzp
    port map (mfnlk => o);
  
  -- Multi-driven assignments
  toxidiz <= 'X';
  o <= ehdmtuobb;
end ld;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (irbod : in integer; w : buffer time; syyiqkllt : in time; ywsf : out std_logic_vector(3 downto 1));
end f;

architecture ofwcaw of f is
  signal oumow : boolean_vector(3 downto 3);
  signal tmhwbzy : time;
  signal jrhbq : real;
  signal wltgkbqt : time;
  signal bprlobd : boolean_vector(3 downto 3);
  signal gwy : time;
  signal cfo : real;
begin
  xjcuncrbh : entity work.kupypmwgli
    port map (g => cfo, k => gwy, dummf => bprlobd, uzwa => wltgkbqt);
  lodqn : entity work.kupypmwgli
    port map (g => jrhbq, k => tmhwbzy, dummf => oumow, uzwa => w);
  
  -- Multi-driven assignments
  ywsf <= "U1H";
  ywsf <= "HWX";
  ywsf <= "XXX";
  ywsf <= ywsf;
end ofwcaw;



-- Seed after: 12555523576152954654,10975587770517081373
