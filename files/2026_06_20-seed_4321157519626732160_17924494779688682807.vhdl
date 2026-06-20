-- Seed: 4321157519626732160,17924494779688682807

entity z is
  port (evpiqbzdq : out real; zgivi : in real; iykxaxz : in character; tnkbesr : inout real);
end z;

architecture ursxtvgre of z is
  
begin
  -- Single-driven assignments
  tnkbesr <= 403.1_4_3_1;
end ursxtvgre;

library ieee;
use ieee.std_logic_1164.all;

entity gpinvn is
  port (nzsi : out integer; wkvsn : in std_logic_vector(4 downto 0); tqrukyx : in integer; egf : inout time);
end gpinvn;

architecture feigizdna of gpinvn is
  signal uc : real;
  signal lukbrbyoa : real;
  signal sdrkjwqr : character;
  signal acvqe : real;
  signal r : real;
  signal l : character;
  signal xquef : real;
  signal cikflmft : real;
begin
  llbggms : entity work.z
    port map (evpiqbzdq => cikflmft, zgivi => xquef, iykxaxz => l, tnkbesr => r);
  cbrzzar : entity work.z
    port map (evpiqbzdq => xquef, zgivi => acvqe, iykxaxz => sdrkjwqr, tnkbesr => lukbrbyoa);
  ny : entity work.z
    port map (evpiqbzdq => uc, zgivi => lukbrbyoa, iykxaxz => l, tnkbesr => acvqe);
  
  -- Single-driven assignments
  l <= 'w';
  sdrkjwqr <= 'o';
end feigizdna;

entity zwqwvb is
  port (wuuvaek : buffer boolean_vector(0 downto 0));
end zwqwvb;

library ieee;
use ieee.std_logic_1164.all;

architecture wgsq of zwqwvb is
  signal wvjlavstxl : real;
  signal baqbc : character;
  signal fhpdkih : real;
  signal m : time;
  signal izftbscn : integer;
  signal anxmnyf : time;
  signal hz : integer;
  signal rjfibov : std_logic_vector(4 downto 0);
  signal lodfsn : integer;
begin
  mopgpqpa : entity work.gpinvn
    port map (nzsi => lodfsn, wkvsn => rjfibov, tqrukyx => hz, egf => anxmnyf);
  uxlzjmg : entity work.gpinvn
    port map (nzsi => izftbscn, wkvsn => rjfibov, tqrukyx => lodfsn, egf => m);
  jqkob : entity work.z
    port map (evpiqbzdq => fhpdkih, zgivi => fhpdkih, iykxaxz => baqbc, tnkbesr => wvjlavstxl);
  
  -- Single-driven assignments
  baqbc <= 'h';
  hz <= 3_3_1;
  wuuvaek <= (others => TRUE);
  
  -- Multi-driven assignments
  rjfibov <= ('L', '0', '1', 'H', '0');
end wgsq;

entity v is
  port (ueudak : out real);
end v;

architecture vlgbfsok of v is
  signal aomjpzmee : character;
  signal yx : real;
  signal jnhhv : boolean_vector(0 downto 0);
  signal eh : boolean_vector(0 downto 0);
begin
  fkrqxumd : entity work.zwqwvb
    port map (wuuvaek => eh);
  rfoudyros : entity work.zwqwvb
    port map (wuuvaek => jnhhv);
  gcbqdqvap : entity work.z
    port map (evpiqbzdq => yx, zgivi => yx, iykxaxz => aomjpzmee, tnkbesr => ueudak);
  
  -- Single-driven assignments
  aomjpzmee <= 'x';
end vlgbfsok;



-- Seed after: 4513806129804368025,17924494779688682807
