-- Seed: 1908834514216532529,662889661651915549

entity srzhljatj is
  port (ndzxxjc : linkage time; j : in time);
end srzhljatj;

architecture hevx of srzhljatj is
  
begin
  
end hevx;

entity ricxethu is
  port (scyo : out boolean_vector(3 downto 1); t : inout time; rztyghf : out real);
end ricxethu;

architecture fjouzvco of ricxethu is
  signal mjwm : time;
  signal ryukgd : time;
begin
  dlil : entity work.srzhljatj
    port map (ndzxxjc => ryukgd, j => mjwm);
  
  -- Single-driven assignments
  scyo <= (TRUE, TRUE, FALSE);
  rztyghf <= rztyghf;
end fjouzvco;

entity qhgyhc is
  port (yxwdt : out integer);
end qhgyhc;

architecture csxvkxixhv of qhgyhc is
  signal jpqchwfb : real;
  signal lxmxsllv : time;
  signal u : boolean_vector(3 downto 1);
  signal ayrnoj : time;
  signal jpuvrsvsk : time;
  signal cwhqwudd : time;
  signal lsp : time;
begin
  mfsjb : entity work.srzhljatj
    port map (ndzxxjc => lsp, j => cwhqwudd);
  jpgmy : entity work.srzhljatj
    port map (ndzxxjc => jpuvrsvsk, j => ayrnoj);
  gyqjmnqg : entity work.srzhljatj
    port map (ndzxxjc => cwhqwudd, j => lsp);
  jlg : entity work.ricxethu
    port map (scyo => u, t => lxmxsllv, rztyghf => jpqchwfb);
end csxvkxixhv;

library ieee;
use ieee.std_logic_1164.all;

entity eahsnqymk is
  port (coqitzegis : out real; ev : inout std_logic; z : in boolean);
end eahsnqymk;

architecture athnnpdn of eahsnqymk is
  signal iuh : integer;
  signal uvguvej : time;
  signal lx : time;
  signal a : time;
  signal cdftujkvf : time;
  signal gefzwnipo : boolean_vector(3 downto 1);
begin
  xm : entity work.ricxethu
    port map (scyo => gefzwnipo, t => cdftujkvf, rztyghf => coqitzegis);
  bjnvgj : entity work.srzhljatj
    port map (ndzxxjc => a, j => lx);
  bofv : entity work.srzhljatj
    port map (ndzxxjc => lx, j => uvguvej);
  unuhcwrst : entity work.qhgyhc
    port map (yxwdt => iuh);
  
  -- Multi-driven assignments
  ev <= '0';
end athnnpdn;



-- Seed after: 42338244218555402,662889661651915549
