-- Seed: 613950444843457537,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity nycnmwfgd is
  port (glqfint : out time; bxfnnmuadt : buffer std_logic_vector(3 downto 0); ib : inout std_logic_vector(2 to 1); x : in boolean_vector(2 downto 1));
end nycnmwfgd;

architecture rhgic of nycnmwfgd is
  
begin
  -- Single-driven assignments
  glqfint <= 2#10111.1_0_0_0# fs;
  
  -- Multi-driven assignments
  ib <= (others => '0');
end rhgic;

library ieee;
use ieee.std_logic_1164.all;

entity nyvuu is
  port (jl : in time; roecp : inout std_logic; sztvwvbssm : inout integer; llqw : inout std_logic);
end nyvuu;

architecture srpuce of nyvuu is
  
begin
  -- Single-driven assignments
  sztvwvbssm <= 03;
  
  -- Multi-driven assignments
  llqw <= 'X';
end srpuce;

entity jpzjeuuc is
  port (wtuffw : inout bit);
end jpzjeuuc;

library ieee;
use ieee.std_logic_1164.all;

architecture gldrx of jpzjeuuc is
  signal wc : boolean_vector(2 downto 1);
  signal ohnwflv : std_logic_vector(2 to 1);
  signal gf : std_logic_vector(3 downto 0);
  signal ixusif : time;
begin
  oq : entity work.nycnmwfgd
    port map (glqfint => ixusif, bxfnnmuadt => gf, ib => ohnwflv, x => wc);
  
  -- Single-driven assignments
  wc <= (TRUE, FALSE);
  wtuffw <= '0';
  
  -- Multi-driven assignments
  ohnwflv <= "";
  gf <= ('X', '0', 'U', 'X');
end gldrx;

entity kxoz is
  port (iplicp : in character; zgn : inout real_vector(4 to 2));
end kxoz;

library ieee;
use ieee.std_logic_1164.all;

architecture rlkmtpuzw of kxoz is
  signal tkyfwpdkg : boolean_vector(2 downto 1);
  signal nkgo : std_logic_vector(2 to 1);
  signal qbnb : time;
  signal f : std_logic_vector(2 to 1);
  signal xfzye : time;
  signal smig : boolean_vector(2 downto 1);
  signal bbtrwia : std_logic_vector(2 to 1);
  signal eedvqdhcf : std_logic_vector(3 downto 0);
  signal uclngos : time;
  signal xwvuwhpr : bit;
begin
  ww : entity work.jpzjeuuc
    port map (wtuffw => xwvuwhpr);
  eirinaoiyb : entity work.nycnmwfgd
    port map (glqfint => uclngos, bxfnnmuadt => eedvqdhcf, ib => bbtrwia, x => smig);
  fvlaglb : entity work.nycnmwfgd
    port map (glqfint => xfzye, bxfnnmuadt => eedvqdhcf, ib => f, x => smig);
  nk : entity work.nycnmwfgd
    port map (glqfint => qbnb, bxfnnmuadt => eedvqdhcf, ib => nkgo, x => tkyfwpdkg);
  
  -- Single-driven assignments
  zgn <= (others => 0.0);
  smig <= (TRUE, TRUE);
  tkyfwpdkg <= (TRUE, FALSE);
  
  -- Multi-driven assignments
  bbtrwia <= "";
  eedvqdhcf <= ('-', 'Z', 'U', 'Z');
  bbtrwia <= (others => '0');
  f <= "";
end rlkmtpuzw;



-- Seed after: 2881460785377350842,4860866131898729603
