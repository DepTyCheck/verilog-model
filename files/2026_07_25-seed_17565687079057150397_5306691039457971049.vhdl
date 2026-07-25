-- Seed: 17565687079057150397,5306691039457971049

entity nbrxxlu is
  port (shfola : inout real_vector(1 to 4));
end nbrxxlu;

architecture h of nbrxxlu is
  
begin
  
end h;

library ieee;
use ieee.std_logic_1164.all;

entity mxsdrjbma is
  port (jwgestsiqo : out std_logic; pobzaxenq : in integer);
end mxsdrjbma;

architecture c of mxsdrjbma is
  signal vt : real_vector(1 to 4);
begin
  ez : entity work.nbrxxlu
    port map (shfola => vt);
  
  -- Multi-driven assignments
  jwgestsiqo <= jwgestsiqo;
  jwgestsiqo <= jwgestsiqo;
  jwgestsiqo <= jwgestsiqo;
  jwgestsiqo <= '0';
end c;

library ieee;
use ieee.std_logic_1164.all;

entity agocddetmc is
  port (niqjrw : inout time; csxqcjv : inout std_logic_vector(1 to 3); ko : buffer severity_level; jvhjrjfvp : out std_logic_vector(0 downto 4));
end agocddetmc;

library ieee;
use ieee.std_logic_1164.all;

architecture d of agocddetmc is
  signal kdea : real_vector(1 to 4);
  signal quccgvwgme : integer;
  signal fzilrg : std_logic;
begin
  mxwmsxlw : entity work.mxsdrjbma
    port map (jwgestsiqo => fzilrg, pobzaxenq => quccgvwgme);
  zupkitel : entity work.nbrxxlu
    port map (shfola => kdea);
  dlyqyhusvy : entity work.mxsdrjbma
    port map (jwgestsiqo => fzilrg, pobzaxenq => quccgvwgme);
  oiqruan : entity work.mxsdrjbma
    port map (jwgestsiqo => fzilrg, pobzaxenq => quccgvwgme);
  
  -- Single-driven assignments
  ko <= ERROR;
  
  -- Multi-driven assignments
  jvhjrjfvp <= jvhjrjfvp;
  csxqcjv <= csxqcjv;
  csxqcjv <= ('L', 'Z', '-');
  jvhjrjfvp <= "";
end d;

entity scrnanim is
  port (s : in real; scgixrkfk : out bit_vector(1 to 0));
end scrnanim;

library ieee;
use ieee.std_logic_1164.all;

architecture uicnyocri of scrnanim is
  signal bwgnbmgv : std_logic_vector(0 downto 4);
  signal xmwfkue : severity_level;
  signal rv : std_logic_vector(1 to 3);
  signal zpmpct : time;
begin
  gvqrc : entity work.agocddetmc
    port map (niqjrw => zpmpct, csxqcjv => rv, ko => xmwfkue, jvhjrjfvp => bwgnbmgv);
  
  -- Single-driven assignments
  scgixrkfk <= (others => '0');
  
  -- Multi-driven assignments
  bwgnbmgv <= (others => '0');
  bwgnbmgv <= (others => '0');
  rv <= ('U', '0', '-');
end uicnyocri;



-- Seed after: 16937062217680638914,5306691039457971049
