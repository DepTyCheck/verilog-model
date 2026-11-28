-- Seed: 3335877786888271434,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity fafrgiidre is
  port (nazglaja : out std_logic; vedrsb : buffer bit; qw : buffer integer; zdmadou : out time_vector(4 to 4));
end fafrgiidre;

architecture iejooe of fafrgiidre is
  
begin
  
end iejooe;

library ieee;
use ieee.std_logic_1164.all;

entity juhy is
  port (nhjqc : out severity_level; nbpewhcq : inout std_logic_vector(1 downto 3); orvrsfj : inout time; essxy : buffer real_vector(4 to 1));
end juhy;

library ieee;
use ieee.std_logic_1164.all;

architecture jouz of juhy is
  signal jdckh : time_vector(4 to 4);
  signal yalmm : integer;
  signal sil : bit;
  signal pdfnu : std_logic;
  signal lhjjrijb : time_vector(4 to 4);
  signal d : integer;
  signal bs : bit;
  signal ssqmwmls : std_logic;
  signal y : time_vector(4 to 4);
  signal ebopr : integer;
  signal qz : bit;
  signal nhxwlgf : std_logic;
begin
  dxskmlxku : entity work.fafrgiidre
    port map (nazglaja => nhxwlgf, vedrsb => qz, qw => ebopr, zdmadou => y);
  ihlecaw : entity work.fafrgiidre
    port map (nazglaja => ssqmwmls, vedrsb => bs, qw => d, zdmadou => lhjjrijb);
  v : entity work.fafrgiidre
    port map (nazglaja => pdfnu, vedrsb => sil, qw => yalmm, zdmadou => jdckh);
  
  -- Single-driven assignments
  nhjqc <= nhjqc;
  
  -- Multi-driven assignments
  pdfnu <= 'L';
  ssqmwmls <= nhxwlgf;
end jouz;

entity rgegqi is
  port (lmltm : out integer);
end rgegqi;

library ieee;
use ieee.std_logic_1164.all;

architecture qkwfas of rgegqi is
  signal zaakpu : time_vector(4 to 4);
  signal qtladfu : integer;
  signal tiinyg : bit;
  signal fzzwapum : std_logic;
  signal pulvadafvx : time_vector(4 to 4);
  signal uuwn : bit;
  signal yeptwhgxr : time_vector(4 to 4);
  signal e : integer;
  signal pohukusbr : bit;
  signal i : std_logic;
begin
  cgkfooy : entity work.fafrgiidre
    port map (nazglaja => i, vedrsb => pohukusbr, qw => e, zdmadou => yeptwhgxr);
  cyhjbenh : entity work.fafrgiidre
    port map (nazglaja => i, vedrsb => uuwn, qw => lmltm, zdmadou => pulvadafvx);
  sipfsck : entity work.fafrgiidre
    port map (nazglaja => fzzwapum, vedrsb => tiinyg, qw => qtladfu, zdmadou => zaakpu);
  
  -- Multi-driven assignments
  i <= '1';
end qkwfas;



-- Seed after: 13110512687405383112,15306146742274979323
