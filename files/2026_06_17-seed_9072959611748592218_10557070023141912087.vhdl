-- Seed: 9072959611748592218,10557070023141912087

entity du is
  port (pzzfa : linkage real; pycjcqsup : inout real_vector(1 to 0); lkwnyv : linkage integer);
end du;

architecture jzr of du is
  
begin
  -- Single-driven assignments
  pycjcqsup <= (others => 0.0);
end jzr;

entity ztvc is
  port (veaswig : in time; qcvfyuscab : out boolean_vector(4 to 3));
end ztvc;

architecture xytnlqdu of ztvc is
  signal sevcpzx : integer;
  signal zmofm : real_vector(1 to 0);
  signal edhogcy : real;
  signal lfsp : integer;
  signal bwy : real_vector(1 to 0);
  signal x : real;
  signal c : integer;
  signal aswc : real_vector(1 to 0);
  signal nuyi : real;
  signal egrirmd : integer;
  signal fgvx : real_vector(1 to 0);
  signal imvdnnyxl : real;
begin
  qz : entity work.du
    port map (pzzfa => imvdnnyxl, pycjcqsup => fgvx, lkwnyv => egrirmd);
  gifmeknc : entity work.du
    port map (pzzfa => nuyi, pycjcqsup => aswc, lkwnyv => c);
  i : entity work.du
    port map (pzzfa => x, pycjcqsup => bwy, lkwnyv => lfsp);
  swgveueua : entity work.du
    port map (pzzfa => edhogcy, pycjcqsup => zmofm, lkwnyv => sevcpzx);
  
  -- Single-driven assignments
  qcvfyuscab <= (others => TRUE);
end xytnlqdu;



-- Seed after: 1224639609584442888,10557070023141912087
