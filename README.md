# ocaml-msx

MSX2+ 에뮬레이터 코어. OCaml. 순수 라이브러리(시간·입출력 없음)로, 클라이언트가
붙는다: `bin/msx_demo`(터미널 데모), masc TUI 스펙테이터 탭, keeper 도구.

상태: 스텁. Z80/V9938 은 아직 없고 프레임은 테스트 패턴이다. 함수 모양은 최종본과
같아서 클라이언트는 지금부터 이 인터페이스를 대상으로 붙는다. 계획은 [ROADMAP.md](ROADMAP.md).

```sh
dune build
dune exec bin/msx_demo.exe              # Ctrl-C 로 종료
dune exec bin/msx_demo.exe -- --frames 3  # 3프레임만 찍고 종료 (CI/확인용)
```

라이선스: MIT. 기계 BIOS 는 C-BIOS(2-clause BSD)만 실는다. 게임 덤프는 사용자 소관.
