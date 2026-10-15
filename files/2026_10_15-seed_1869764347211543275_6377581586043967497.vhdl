-- Seed: 1869764347211543275,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity kuqxoug is
  port (pasbsy : buffer std_logic_vector(2 to 1); cjw : buffer real; pvblhuyhh : linkage time; i : inout integer);
end kuqxoug;

architecture abvlc of kuqxoug is
  
begin
  -- Single-driven assignments
  i <= 0;
  cjw <= 2#011.0_1_0_0#;
end abvlc;

library ieee;
use ieee.std_logic_1164.all;

entity tlshhw is
  port (vcsup : buffer integer; yxbfrtcc : inout time_vector(1 to 4); hxz : inout time; cvox : in std_logic_vector(4 downto 0));
end tlshhw;

library ieee;
use ieee.std_logic_1164.all;

architecture t of tlshhw is
  signal r : real;
  signal q : std_logic_vector(2 to 1);
  signal rwjkhqasy : integer;
  signal dmwk : time;
  signal gyctind : real;
  signal jmpu : integer;
  signal rju : time;
  signal auyrquuxq : real;
  signal ihszvm : std_logic_vector(2 to 1);
  signal fut : integer;
  signal xa : time;
  signal lhmrm : real;
  signal abwig : std_logic_vector(2 to 1);
begin
  xukrtaft : entity work.kuqxoug
    port map (pasbsy => abwig, cjw => lhmrm, pvblhuyhh => xa, i => fut);
  grpmntw : entity work.kuqxoug
    port map (pasbsy => ihszvm, cjw => auyrquuxq, pvblhuyhh => rju, i => jmpu);
  jdqvu : entity work.kuqxoug
    port map (pasbsy => abwig, cjw => gyctind, pvblhuyhh => dmwk, i => rwjkhqasy);
  tnwfl : entity work.kuqxoug
    port map (pasbsy => q, cjw => r, pvblhuyhh => hxz, i => vcsup);
  
  -- Single-driven assignments
  yxbfrtcc <= (2#0_0_0_0# ns, 8#3_5_2.12016# fs, 0 sec, 2 ns);
  
  -- Multi-driven assignments
  abwig <= abwig;
  ihszvm <= "";
end t;

entity pxemtnp is
  port (y : buffer real);
end pxemtnp;

library ieee;
use ieee.std_logic_1164.all;

architecture snsgkgsb of pxemtnp is
  signal foyvlzhywa : std_logic_vector(4 downto 0);
  signal bf : time;
  signal c : time_vector(1 to 4);
  signal omkmfxzpf : integer;
begin
  nqmuju : entity work.tlshhw
    port map (vcsup => omkmfxzpf, yxbfrtcc => c, hxz => bf, cvox => foyvlzhywa);
  
  -- Single-driven assignments
  y <= 8#227.3_4#;
  
  -- Multi-driven assignments
  foyvlzhywa <= ('W', '-', 'W', 'W', 'X');
  foyvlzhywa <= ('Z', '1', 'Z', 'W', 'L');
  foyvlzhywa <= foyvlzhywa;
  foyvlzhywa <= ('-', 'X', 'Z', '0', '0');
end snsgkgsb;



-- Seed after: 14842555226097287205,6377581586043967497
