-- Seed: 20796349956718117,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity bvouod is
  port (mjqczgr : buffer std_logic_vector(4 downto 1); bfc : out bit; kcpjm : buffer string(5 to 2); uf : linkage std_logic_vector(3 to 1));
end bvouod;

architecture le of bvouod is
  
begin
  -- Single-driven assignments
  kcpjm <= (others => ' ');
  bfc <= bfc;
  
  -- Multi-driven assignments
  mjqczgr <= ('0', 'U', 'L', '1');
  mjqczgr <= "0XWH";
  mjqczgr <= ('1', '1', '0', 'U');
end le;

library ieee;
use ieee.std_logic_1164.all;

entity jpxznyt is
  port (vun : out std_logic_vector(0 downto 1); egdlgti : out std_logic; ey : inout std_logic_vector(2 downto 4); wp : inout boolean);
end jpxznyt;

architecture jxkwgxl of jpxznyt is
  
begin
  -- Single-driven assignments
  wp <= TRUE;
  
  -- Multi-driven assignments
  ey <= "";
  ey <= vun;
  vun <= "";
end jxkwgxl;

library ieee;
use ieee.std_logic_1164.all;

entity lagn is
  port (ommynvgy : in std_logic; ukwwa : in integer; nenpvxnpld : out std_logic_vector(1 downto 2); vskea : inout bit);
end lagn;

library ieee;
use ieee.std_logic_1164.all;

architecture cn of lagn is
  signal dilrxlugag : string(5 to 2);
  signal qiakda : bit;
  signal hkzn : string(5 to 2);
  signal vxqazndotf : bit;
  signal oreclbp : std_logic_vector(4 downto 1);
  signal tmqhszibyr : string(5 to 2);
  signal cfcjd : bit;
  signal tbkdekdy : std_logic_vector(4 downto 1);
  signal qvexktexo : boolean;
  signal twulvbsqc : std_logic;
begin
  yahjz : entity work.jpxznyt
    port map (vun => nenpvxnpld, egdlgti => twulvbsqc, ey => nenpvxnpld, wp => qvexktexo);
  dgdfuew : entity work.bvouod
    port map (mjqczgr => tbkdekdy, bfc => cfcjd, kcpjm => tmqhszibyr, uf => nenpvxnpld);
  rig : entity work.bvouod
    port map (mjqczgr => oreclbp, bfc => vxqazndotf, kcpjm => hkzn, uf => nenpvxnpld);
  jopbvppqgj : entity work.bvouod
    port map (mjqczgr => tbkdekdy, bfc => qiakda, kcpjm => dilrxlugag, uf => nenpvxnpld);
  
  -- Single-driven assignments
  vskea <= vskea;
  
  -- Multi-driven assignments
  twulvbsqc <= ommynvgy;
  nenpvxnpld <= nenpvxnpld;
  tbkdekdy <= ('L', '1', '0', 'H');
end cn;



-- Seed after: 4133405898401828413,7311216359267151659
