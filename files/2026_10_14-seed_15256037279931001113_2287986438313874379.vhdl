-- Seed: 15256037279931001113,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port ( wtaban : out std_logic
  ; ohxa : out std_logic_vector(2 to 4)
  ; psamlkib : linkage time_vector(0 to 4)
  ; azcktzsoq : out std_logic_vector(2 downto 1)
  );
end n;

architecture lj of n is
  
begin
  -- Multi-driven assignments
  wtaban <= 'X';
  azcktzsoq <= ('1', 'Z');
end lj;

library ieee;
use ieee.std_logic_1164.all;

entity cqhjyjpi is
  port (uqpzppdezd : linkage time; zokif : linkage std_logic_vector(0 downto 3); vavpvivomg : buffer integer; rsmk : buffer severity_level);
end cqhjyjpi;

library ieee;
use ieee.std_logic_1164.all;

architecture olm of cqhjyjpi is
  signal vwhdti : std_logic_vector(2 downto 1);
  signal qtpug : time_vector(0 to 4);
  signal lwajoq : std_logic_vector(2 to 4);
  signal kuas : std_logic;
  signal wm : std_logic_vector(2 downto 1);
  signal lblgdjfuke : time_vector(0 to 4);
  signal bxjop : std_logic_vector(2 to 4);
  signal us : std_logic;
begin
  fzmcb : entity work.n
    port map (wtaban => us, ohxa => bxjop, psamlkib => lblgdjfuke, azcktzsoq => wm);
  pzfq : entity work.n
    port map (wtaban => kuas, ohxa => lwajoq, psamlkib => qtpug, azcktzsoq => vwhdti);
  
  -- Single-driven assignments
  rsmk <= WARNING;
  vavpvivomg <= vavpvivomg;
end olm;

entity g is
  port (thwxber : buffer integer_vector(0 to 1));
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture oifizwue of g is
  signal opqgxfaxmn : std_logic_vector(2 downto 1);
  signal dafr : time_vector(0 to 4);
  signal mmygwerl : std_logic_vector(2 to 4);
  signal j : std_logic;
begin
  lftn : entity work.n
    port map (wtaban => j, ohxa => mmygwerl, psamlkib => dafr, azcktzsoq => opqgxfaxmn);
  
  -- Multi-driven assignments
  j <= '0';
  mmygwerl <= mmygwerl;
  j <= 'Z';
end oifizwue;

library ieee;
use ieee.std_logic_1164.all;

entity qatqpm is
  port (pwg : inout std_logic_vector(1 downto 2); httropjxt : inout std_logic);
end qatqpm;

library ieee;
use ieee.std_logic_1164.all;

architecture zolosu of qatqpm is
  signal klxznsuofy : std_logic_vector(2 downto 1);
  signal qmkp : time_vector(0 to 4);
  signal mbqzq : std_logic_vector(2 to 4);
  signal wttisxf : integer_vector(0 to 1);
  signal jve : time_vector(0 to 4);
  signal hgrofkbmt : std_logic_vector(2 to 4);
  signal k : std_logic;
  signal e : std_logic_vector(2 downto 1);
  signal mul : time_vector(0 to 4);
  signal pqnjtzaubg : std_logic_vector(2 to 4);
  signal ggkjvgfaub : std_logic;
begin
  hdgzvgs : entity work.n
    port map (wtaban => ggkjvgfaub, ohxa => pqnjtzaubg, psamlkib => mul, azcktzsoq => e);
  vxuanp : entity work.n
    port map (wtaban => k, ohxa => hgrofkbmt, psamlkib => jve, azcktzsoq => e);
  rbbxp : entity work.g
    port map (thwxber => wttisxf);
  nyqg : entity work.n
    port map (wtaban => httropjxt, ohxa => mbqzq, psamlkib => qmkp, azcktzsoq => klxznsuofy);
  
  -- Multi-driven assignments
  httropjxt <= '0';
  mbqzq <= ('-', '1', 'H');
end zolosu;



-- Seed after: 1856254905066865188,2287986438313874379
