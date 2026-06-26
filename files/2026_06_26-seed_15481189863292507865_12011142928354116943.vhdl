-- Seed: 15481189863292507865,12011142928354116943

entity jsfvqu is
  port (nxaa : buffer bit_vector(0 downto 1); wgdc : in time_vector(3 downto 2); mlxwfjofnx : buffer boolean_vector(4 to 3));
end jsfvqu;

architecture etp of jsfvqu is
  
begin
  -- Single-driven assignments
  mlxwfjofnx <= (others => TRUE);
  nxaa <= (others => '0');
end etp;

entity o is
  port (pjtls : inout time);
end o;

architecture iqesgup of o is
  signal kfzamzf : boolean_vector(4 to 3);
  signal ezungly : time_vector(3 downto 2);
  signal smjdacb : bit_vector(0 downto 1);
begin
  oevjaaoceo : entity work.jsfvqu
    port map (nxaa => smjdacb, wgdc => ezungly, mlxwfjofnx => kfzamzf);
  
  -- Single-driven assignments
  pjtls <= 3 sec;
  ezungly <= (16#5# ns, 2#1.111# ps);
end iqesgup;

entity uslwode is
  port (fkcph : out integer);
end uslwode;

architecture rnq of uslwode is
  signal rxofevowd : time;
begin
  osxckvw : entity work.o
    port map (pjtls => rxofevowd);
  
  -- Single-driven assignments
  fkcph <= 4_0;
end rnq;

entity jzsjiqa is
  port (iwwq : in bit; ae : inout time);
end jzsjiqa;

architecture ig of jzsjiqa is
  signal vvtupnpk : time;
begin
  tpbprn : entity work.o
    port map (pjtls => vvtupnpk);
  uh : entity work.o
    port map (pjtls => ae);
end ig;



-- Seed after: 7106442578049003143,12011142928354116943
