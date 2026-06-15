-- Seed: 12849540937678962323,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity th is
  port (j : out real; p : buffer time; laxplfwl : out boolean_vector(3 downto 2); oapgrckb : in std_logic_vector(0 downto 1));
end th;

architecture dmo of th is
  
begin
  -- Single-driven assignments
  laxplfwl <= (FALSE, TRUE);
end dmo;

entity isvvtu is
  port (bptjgx : inout integer; dtzhhpjkel : in real);
end isvvtu;

library ieee;
use ieee.std_logic_1164.all;

architecture duf of isvvtu is
  signal owoyxak : std_logic_vector(0 downto 1);
  signal i : boolean_vector(3 downto 2);
  signal rsnrrambcp : time;
  signal xhggtwbaim : real;
  signal vcglwmmke : std_logic_vector(0 downto 1);
  signal elyc : boolean_vector(3 downto 2);
  signal bhuqveo : time;
  signal qguvpjqec : real;
  signal a : std_logic_vector(0 downto 1);
  signal qdbcnayly : boolean_vector(3 downto 2);
  signal rnyybk : time;
  signal frltmu : real;
  signal zljenwcxvp : std_logic_vector(0 downto 1);
  signal rlyewoa : boolean_vector(3 downto 2);
  signal dhpceimmk : time;
  signal xk : real;
begin
  pvwrlrc : entity work.th
    port map (j => xk, p => dhpceimmk, laxplfwl => rlyewoa, oapgrckb => zljenwcxvp);
  t : entity work.th
    port map (j => frltmu, p => rnyybk, laxplfwl => qdbcnayly, oapgrckb => a);
  syrt : entity work.th
    port map (j => qguvpjqec, p => bhuqveo, laxplfwl => elyc, oapgrckb => vcglwmmke);
  vt : entity work.th
    port map (j => xhggtwbaim, p => rsnrrambcp, laxplfwl => i, oapgrckb => owoyxak);
  
  -- Single-driven assignments
  bptjgx <= 4241;
  
  -- Multi-driven assignments
  vcglwmmke <= "";
  zljenwcxvp <= (others => '0');
end duf;

entity kgp is
  port (sygeeqws : out real);
end kgp;

architecture tbwqkazjt of kgp is
  
begin
  -- Single-driven assignments
  sygeeqws <= 114.2;
end tbwqkazjt;

entity exf is
  port (gaczwveh : in real; lgw : in time);
end exf;

library ieee;
use ieee.std_logic_1164.all;

architecture czeuicxpjq of exf is
  signal dqw : boolean_vector(3 downto 2);
  signal wnygw : time;
  signal qv : real;
  signal kmeypsxc : std_logic_vector(0 downto 1);
  signal bhch : boolean_vector(3 downto 2);
  signal hwflusfj : time;
  signal rlbt : real;
begin
  zac : entity work.th
    port map (j => rlbt, p => hwflusfj, laxplfwl => bhch, oapgrckb => kmeypsxc);
  mbzljgvx : entity work.th
    port map (j => qv, p => wnygw, laxplfwl => dqw, oapgrckb => kmeypsxc);
  
  -- Multi-driven assignments
  kmeypsxc <= (others => '0');
  kmeypsxc <= "";
  kmeypsxc <= (others => '0');
  kmeypsxc <= (others => '0');
end czeuicxpjq;



-- Seed after: 2951317158746715567,15300320181035395489
