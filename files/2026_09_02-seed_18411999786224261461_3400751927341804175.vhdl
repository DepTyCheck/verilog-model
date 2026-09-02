-- Seed: 18411999786224261461,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity sqipxnsump is
  port (tnqkbpziwx : buffer bit; oxp : linkage time; zvajbvpy : linkage std_logic; c : linkage time);
end sqipxnsump;

architecture rmtp of sqipxnsump is
  
begin
  -- Single-driven assignments
  tnqkbpziwx <= '1';
end rmtp;

entity zwmoz is
  port (dgy : in boolean_vector(1 downto 2); l : inout real);
end zwmoz;

library ieee;
use ieee.std_logic_1164.all;

architecture rcqmmzyyc of zwmoz is
  signal lahwgxcb : time;
  signal uponsinb : std_logic;
  signal tcbgn : time;
  signal aietdersq : bit;
  signal qu : time;
  signal whmeufy : std_logic;
  signal utkm : time;
  signal alhdhul : bit;
begin
  kumcq : entity work.sqipxnsump
    port map (tnqkbpziwx => alhdhul, oxp => utkm, zvajbvpy => whmeufy, c => qu);
  qm : entity work.sqipxnsump
    port map (tnqkbpziwx => aietdersq, oxp => tcbgn, zvajbvpy => uponsinb, c => lahwgxcb);
  
  -- Single-driven assignments
  l <= 8#76676.70774#;
end rcqmmzyyc;

library ieee;
use ieee.std_logic_1164.all;

entity xxuflvz is
  port (lzrcvvtiy : inout std_logic; qxgjcmvdif : out integer; moodjghr : inout integer; sazy : out time);
end xxuflvz;

library ieee;
use ieee.std_logic_1164.all;

architecture y of xxuflvz is
  signal pjtjdlrya : time;
  signal su : std_logic;
  signal mhanghkvi : time;
  signal yocmxxm : bit;
  signal bhvsfcnwyz : real;
  signal ayqhk : boolean_vector(1 downto 2);
  signal ivpkjc : real;
  signal nlzqxmv : boolean_vector(1 downto 2);
begin
  jscp : entity work.zwmoz
    port map (dgy => nlzqxmv, l => ivpkjc);
  eohuf : entity work.zwmoz
    port map (dgy => ayqhk, l => bhvsfcnwyz);
  ksy : entity work.sqipxnsump
    port map (tnqkbpziwx => yocmxxm, oxp => mhanghkvi, zvajbvpy => su, c => pjtjdlrya);
  
  -- Multi-driven assignments
  lzrcvvtiy <= lzrcvvtiy;
  lzrcvvtiy <= '-';
end y;

library ieee;
use ieee.std_logic_1164.all;

entity ibzf is
  port (xpigglt : out std_logic_vector(0 downto 1));
end ibzf;

architecture bz of ibzf is
  signal hsptnuhucp : real;
  signal xpidmqejvc : boolean_vector(1 downto 2);
begin
  dry : entity work.zwmoz
    port map (dgy => xpidmqejvc, l => hsptnuhucp);
  
  -- Single-driven assignments
  xpidmqejvc <= (others => TRUE);
  
  -- Multi-driven assignments
  xpigglt <= "";
  xpigglt <= (others => '0');
  xpigglt <= xpigglt;
end bz;



-- Seed after: 16883097044908278587,3400751927341804175
